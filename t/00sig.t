use lib 'inc';
use Test::More tests => 1;
use Test::Signature;

$@ = undef;
eval {
	require Module::Signature;
	die "Module::Signature version number too low."
		if $Module::Signature::VERSION lt '0.66';
};
if ($@)
{
	diag $@;
	ok(1, "Skipping SIGNATURE test.");
	exit;
}

signature_ok();
