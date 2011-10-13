use lib "lib";
use RDF::RDFa::Parser;

use Data::Dumper;
#print Dumper( [RDF::RDFa::Parser::Profile->plugins] );

my $xml = <<'XML';
<html version="RDFa 1.1">
<p>
	<span role="contentinfo" rev="made" resource="maker" property="prev">x</span>
</p>
</html>
XML

my $p = RDF::RDFa::Parser->new($xml, "http://example.com/",
	RDF::RDFa::Parser::Config->new('html4', 'guess', role_attr=>1, default_profiles=>'urn:x-internal:profile:xhtml-role urn:x-internal:profile:html32'));
print "### RDFa ".$p->{options}{_rdfa}."\n";
$p->set_callbacks({
	pretriple_resource => 'print',
	pretriple_literal  => 'print',
	ontoken => sub {
		if (defined $_[3])
		{
			print "# Token: '$_[2]' => <$_[3]>\n";
		}
		else
		{
			print "# Token: '$_[2]' => undef\n";
		}
		return $_[3];
		},
	});
$p->consume;


package RDF::RDFa::Parser::Profile::Lala;

use base qw[RDF::RDFa::Parser::Profile];

sub new
{
	return bless [], $_[0]
		if $_[1] eq 'http://example.net/profile';
}

sub get_terms
{
	return ([
		'lala-land',
		'http://example.net/lala-land',
		0, # a case-sensitive term
		'rev'
		]);
}

1;