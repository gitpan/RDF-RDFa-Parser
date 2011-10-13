use lib "lib";
use File::Slurp qw'slurp';
use RDF::RDFa::Parser;
use RDF::TrineShortcuts;

my $data = slurp('example6.odt');
my $p = RDF::RDFa::Parser->new($data, 'http://example.com/example6.odt',
	RDF::RDFa::Parser::Config->new( RDF::RDFa::Parser::Config->HOST_OPENDOCUMENT_ZIP, '1.1' )
	);

my $iter = $p->graph->get_statements(undef,undef,undef,undef);
while (my $st = $iter->next)
{
	print $st->as_string . "\n";
}
