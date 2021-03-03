use Mojo::Base -strict;
use JSON::Validator::Schema::Draft7;
use Test::Deep;
use Test::More;

plan skip_all => 'TEST_STRUCTURE=1' unless $ENV{TEST_STRUCTURE};

my $cwd = Mojo::File->new(__FILE__)->dirname;
my $id  = $cwd->child('spec/more-bundle.yaml')->to_abs;

my $schema = JSON::Validator::Schema::Draft7->new;
$schema->store->load($id);

my @exp_ids = map { to_id($_) } qw(spec/more-bundle.yaml spec/more-bundle2.yaml);
$schema->id($exp_ids[0])->data($schema->store->get($exp_ids[0]));
is $schema->id, $exp_ids[0], 'got correct id';

is int(keys %{$schema->refs}), 16, 'found all local refs';
cmp_deeply(
  $schema->refs->{'/definitions/i_am_a_ref_with_the_same_name/$ref'},
  {
    base => $exp_ids[1],
    fqn  => check_url("$exp_ids[1]#/definitions/i_am_a_ref_with_the_same_name"),
    ref  => 'more-bundle2.yaml#/definitions/i_am_a_ref_with_the_same_name',
  },
  'local ref data'
);

test_ids($schema, [$exp_ids[0]], 'before resolve');
is $schema->resolve, $schema, 'resolve';
test_ids($schema, [@exp_ids], 'after resolve');

is_deeply $schema->errors, [], 'valid';

push @exp_ids, 'http://json-schema.org/draft-07/schema';
test_ids($schema, [@exp_ids], 'after validate');

done_testing;

sub to_id { sprintf 'file://%s', $cwd->child(shift)->to_abs }

sub test_ids {
  my ($schema, $exp, $desc) = @_;
  my @got_ids = sort keys %{$schema->store->schemas};
  is_deeply \@got_ids, $exp, $desc or diag "@got_ids";
}

sub check_url {
  my $exp = shift;

  return code(sub {
    my $url = shift;
    return 0, 'no base url' unless $url->base->to_string;
    return 0, "does not match $exp" if $url->to_abs->to_string ne $exp;
    return 1;
  });
}
