use Test::More tests => 6;
BEGIN { use_ok('RDF::RDFa::Parser') };

use RDF::RDFa::Parser;

my $xhtml = <<EOF;
<html xmlns:foaf="http://xmlns.com/foaf/0.1/">
	<body xmlns:dc="http://purl.org/dc/elements/1.1/">
		<div rel="foaf:primaryTopic" rev="foaf:page">
			<h1 about="#topic" typeof="foaf:Person" property="foaf:name" 
                datatype="http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"><strong>Albert Einstein</strong></h1>
		</div>
	</body>
</html>
EOF
$parser = RDF::RDFa::Parser->new($xhtml, 'http://example.com/einstein');

ok(lc($parser->dom->documentElement->tagName) eq 'html', 'DOM Tree returned OK.');

ok($parser->consume, "Parse OK");

ok(my $graph = $parser->graph, "Graph retrieved");

ok($graph->{'http://example.com/einstein#topic'}->{'http://xmlns.com/foaf/0.1/name'}->[0]->{'datatype'} =~ /XMLLiteral$/ , "XML seems to have correct datatype");

ok($graph->{'http://example.com/einstein#topic'}->{'http://xmlns.com/foaf/0.1/name'}->[0]->{'value'} =~ /^<strong/ , "XML seems to have correct value");

