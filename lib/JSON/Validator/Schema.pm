package JSON::Validator::Schema;
use Mojo::Base 'JSON::Validator';    # TODO: Change this to "use Mojo::Base -base"

use Carp 'carp';
use JSON::Validator::Util qw(E is_type schema_type);
use Mojo::JSON::Pointer;
use Mojo::URL;
use Scalar::Util qw(blessed refaddr);

has errors => sub {
  my $self      = shift;
  my $url       = $self->specification || 'http://json-schema.org/draft-04/schema#';
  my $validator = $self->new(%$self)->id($url)->resolve($url);

  return [$validator->validate($self->resolve->data)];
};

has id => sub {
  my $data = shift->data;
  return is_type($data, 'HASH') ? $data->{'$id'} || $data->{id} || '' : '';
};

has moniker => sub {
  my $self = shift;
  return "draft$1" if $self->specification =~ m!draft-(\d+)!;
  return '';
};

has refs => sub { shift->_build_refs };

has specification => sub {
  my $data = shift->data;
  is_type($data, 'HASH') ? $data->{'$schema'} || $data->{schema} || '' : '';
};

sub bundle {
  my $self   = shift;
  my $params = shift || {};
  return $self->new(%$self)->data($self->SUPER::bundle({schema => $self, %$params}));
}

sub contains {
  state $p = Mojo::JSON::Pointer->new;
  return $p->data(shift->{data})->contains(@_);
}

sub data {
  my $self = shift;
  return $self->{data} //= {} unless @_;
  $self->{data} = shift;
  $self->refs($self->_build_refs);
  delete $self->{errors};
  return $self;
}

sub get {
  state $p = Mojo::JSON::Pointer->new;
  return $p->data(shift->{data})->get(@_) if @_ == 2 and ref $_[1] ne 'ARRAY';
  return JSON::Validator::Util::schema_extract(shift->data, @_);
}

sub load_and_validate_schema { Carp::confess('load_and_validate_schema(...) is unsupported.') }

sub new {
  return shift->SUPER::new(@_) if @_ % 2;
  my ($class, $data) = (shift, shift);
  return $class->SUPER::new(@_)->resolve($data);
}

sub resolve {
  my $self   = shift;
  my $source = @_ ? shift : $self->data;

  my $loadable
    = (blessed $source && ($source->can('scheme') || $source->isa('Mojo::File')))
    || ($source !~ /\n/ && -f $source)
    || (!ref $source && $source =~ /^\w/);

  if ($loadable) {
    my $id = $self->store->load($source);
    $self->id($id) unless $self->{id};
    $self->data($self->store->get($id));
  }
  elsif ($source) {
    $self->data($source);
  }

  for my $ref (values %{$self->refs}) {
    $self->store->load($ref->{base}) if $ref->{base} && !$self->store->get($ref->{base});
  }

  return $self;
}

sub schema { $_[0]->can('data') ? $_[0] : $_[0]->SUPER::schema }

sub validate {
  my $self = $_[0];
  local $self->{seen}        = {};
  local $self->{temp_schema} = [];    # make sure random-errors.t does not fail
  my @e = sort { $a->path cmp $b->path } $self->_validate($_[1], '', $self->data);
  return @e;                          # Force sort() to return a list
}

sub _build_refs {
  my $self = shift;

  my $base_url = Mojo::URL->new($self->id);
  my (@topics, %refs) = (['', $self->data]);
  while (@topics) {
    my $topic = shift @topics;

    if (ref $topic->[1] eq 'ARRAY') {
      my $i = 0;
      push @topics, [sprintf('%s/%s', $topic->[1], $i++), $_] for @{$topic->[1]};
    }
    elsif (ref $topic->[1] eq 'HASH') {
      for my $k (sort keys %{$topic->[1]}) {
        if ($k eq '$ref' and !ref $topic->[1]{'$ref'}) {
          $refs{$topic->[1]{'$ref'}} = $self->_resolve_ref($base_url, $topic->[1]{'$ref'});
        }
        else {
          unshift @topics, ["$topic->[0]/$k", $topic->[1]{$k}];
        }
      }
    }
  }

  return \%refs;
}

sub _definitions_path_for_ref { ['definitions'] }

sub _id_key {'id'}

sub _register_root_schema {
  my ($self, $id, $schema) = @_;
  $self->SUPER::_register_root_schema($id => $schema);
  $self->id($id) unless $self->id;
}

sub _resolve_ref {
  my ($self, $base_url, $ref_url) = @_;
  $ref_url = "#$ref_url" if $ref_url =~ m!^/!;

  my $other_fqn = Mojo::URL->new($ref_url);
  $other_fqn = $other_fqn->to_abs($base_url) if "$base_url";

  my $other_base = $other_fqn->clone->fragment(undef);
  my $other;
  $other //= $self if Mojo::URL->new($self->id)->fragment(undef) eq $other_base;
  $other //= $self->store->get_obj($other_base);
  $other //= $self->store->get_obj($self->store->load($other_base));

  my $ref_schema = $other->get($other_fqn->fragment);
  return {path => $other_fqn->fragment, ref_schema => $ref_schema, schema => $other};
}

sub _validate {
  my ($self, $data, $path, $schema) = @_;
  return $schema ? () : E $path, [not => 'not'] if is_type $schema, 'BOOL';

  if ($schema->{'$ref'} and my $ref = $self->refs->{$schema->{'$ref'}}) {
    return $ref->{schema}->_validate($_[1], $path, $ref->{ref_schema});
  }

  my @errors;
  if ($self->recursive_data_protection) {
    my $seen_addr = join ':', refaddr($schema), (ref $data ? refaddr $data : ++$self->{seen}{scalar});
    return @{$self->{seen}{$seen_addr}} if $self->{seen}{$seen_addr};    # Avoid recursion
    $self->{seen}{$seen_addr} = \@errors;
  }

  local $_[1] = $data->TO_JSON if blessed $data and $data->can('TO_JSON');

  if (my $rules = $schema->{not}) {
    my @e = $self->_validate($_[1], $path, $rules);
    push @errors, E $path, [not => 'not'] unless @e;
  }
  if (my $rules = $schema->{allOf}) {
    push @errors, $self->_validate_all_of($_[1], $path, $rules);
  }
  if (my $rules = $schema->{anyOf}) {
    push @errors, $self->_validate_any_of($_[1], $path, $rules);
  }
  if (my $rules = $schema->{oneOf}) {
    push @errors, $self->_validate_one_of($_[1], $path, $rules);
  }
  if (exists $schema->{if}) {
    my $rules = !$schema->{if} || $self->_validate($_[1], $path, $schema->{if}) ? $schema->{else} : $schema->{then};
    push @errors, $self->_validate($_[1], $path, $rules // {});
  }

  my $type = $schema->{type} || schema_type $schema, $_[1];
  if (ref $type eq 'ARRAY') {
    push @{$self->{temp_schema}}, [map { +{%$schema, type => $_} } @$type];
    push @errors, $self->_validate_any_of_types($_[1], $path, $self->{temp_schema}[-1]);
  }
  elsif ($type) {
    my $method = sprintf '_validate_type_%s', $type;
    push @errors, $self->$method($_[1], $path, $schema);
  }

  return @errors if @errors;

  if (exists $schema->{const}) {
    push @errors, $self->_validate_type_const($_[1], $path, $schema);
  }
  if ($schema->{enum}) {
    push @errors, $self->_validate_type_enum($_[1], $path, $schema);
  }

  return @errors;
}


1;

=encoding utf8

=head1 NAME

JSON::Validator::Schema - Base class for JSON::Validator schemas

=head1 SYNOPSIS

  package JSON::Validator::Schema::SomeSchema;
  use Mojo::Base "JSON::Validator::Schema";
  has specification => "https://api.example.com/my/spec.json#";
  1;

=head1 DESCRIPTION

L<JSON::Validator::Schema> is the base class for
L<JSON::Validator::Schema::Draft4>,
L<JSON::Validator::Schema::Draft6>,
L<JSON::Validator::Schema::Draft7> and
L<JSON::Validator::Schema::Draft201909>.

L<JSON::Validator::Schema> is currently EXPERIMENTAL, and most probably will
change over the next versions as
L<https://github.com/mojolicious/json-validator/pull/189> (or a competing PR)
evolves.

=head1 ATTRIBUTES

=head2 errors

  my $array_ref = $schema->errors;

Holds the errors after checking L</data> against L</specification>.
C<$array_ref> containing no elements means L</data> is valid. Each element in
the array-ref is a L<JSON::Validator::Error> object.

This attribute is I<not> changed by L</validate>. It only reflects if the
C<$schema> is valid.

=head2 id

  my $str    = $schema->id;
  my $schema = $schema->id($str);

Holds the ID for this schema. Usually extracted from C<"$id"> or C<"id"> in
L</data>.

=head2 moniker

  $str    = $schema->moniker;
  $schema = $self->moniker("some_name");

Used to get/set the moniker for the given schema. Will be "draft04" if
L</specification> points to a JSON Schema draft URL, and fallback to
empty string if unable to guess a moniker name.

This attribute will (probably) detect more monikers from a given
L</specification> or C</id> in the future.

=head2 specification

  my $str    = $schema->specification;
  my $schema = $schema->specification($str);

The URL to the specification used when checking for L</errors>. Usually
extracted from C<"$schema"> or C<"schema"> in L</data>.

=head1 METHODS

=head2 bundle

  my $bundled = $schema->bundle;

C<$bundled> is a new L<JSON::Validator::Schema> object where none of the "$ref"
will point to external resources. This can be useful, if you want to have a
bunch of files locally, but hand over a single file to a client.

  Mojo::File->new("client.json")
    ->spurt(Mojo::JSON::to_json($schema->bundle->data));

=head2 coerce

  my $schema   = $schema->coerce("booleans,defaults,numbers,strings");
  my $schema   = $schema->coerce({booleans => 1});
  my $hash_ref = $schema->coerce;

Set the given type to coerce. Before enabling coercion this module is very
strict when it comes to validating types. Example: The string C<"1"> is not
the same as the number C<1>. Note that it will also change the internal
data-structure of the validated data: Example:

  $schema->coerce({numbers => 1});
  $schema->data({properties => {age => {type => "integer"}}});

  my $input = {age => "42"};
  $schema->validate($input);
  # $input->{age} is now an integer 42 and not the string "42"

=head2 contains

See L<Mojo::JSON::Pointer/contains>.

=head2 data

  my $hash_ref = $schema->data;
  my $schema   = $schema->data($bool);
  my $schema   = $schema->data($hash_ref);
  my $schema   = $schema->data($url);

Will set a structure representing the schema. In most cases you want to
use L</resolve> instead of L</data>.

=head2 get

  my $data = $schema->get($json_pointer);
  my $data = $schema->get($json_pointer, sub { my ($data, $json_pointer) = @_; });

Called with one argument, this method acts like L<Mojo::JSON::Pointer/get>,
while if called with two arguments it will work like
L<JSON::Validator::Util/schema_extract> instead:

  JSON::Validator::Util::schema_extract($schema->data, sub { ... });

The second argument can be C<undef()>, if you don't care about the callback.

See L<Mojo::JSON::Pointer/get>.

=head2 load_and_validate_schema

This method will be removed in a future release.

=head2 new

  my $schema = JSON::Validator::Schema->new($data);
  my $schema = JSON::Validator::Schema->new($data, %attributes);
  my $schema = JSON::Validator::Schema->new(%attributes);

Construct a new L<JSON::Validator::Schema> object. Passing on C<$data> as the
first argument will cause L</resolve> to be called, meaning the constructor
might throw an exception if the schema could not be successfully resolved.

=head2 resolve

  $schema = $schema->resolve;
  $schema = $schema->resolve($data);

Used to resolve L</data> or C<$data> and store the resolved schema in L</data>.
If C<$data> is an C<$url> on contains "$ref" pointing to an URL, then these
schemas will be downloaded and resolved as well.

=head2 schema

This method will be removed in a future release.

=head2 validate

  my @errors = $schema->validate($any);

Will validate C<$any> against the schema defined in L</data>. Each element in
C<@errors> is a L<JSON::Validator::Error> object.

=head1 SEE ALSO

L<JSON::Validator>.

=cut
