# Tests that lang tags work properly

use Test::More tests => 8;
BEGIN { use_ok('RDF::RDFa::Parser') };

my $xhtml = <<EOF;
<html xmlns:ex="http://example.com/ns#"
	xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	xmlns="http://www.w3.org/1999/xhtml"
	xml:lang="en-gb">
	<body>
		<div about="[ex:r1]" property="ex:test" datatype="rdf:XMLLiteral">
			<!-- This is just a literal. Should not parse the RDF inside here. -->
			<rdf:RDF>
				<rdf:Description rdf:about="http://example.com/ns#r2">
					<ex:test rdf:resource="http://example.com/ns#test" />
				</rdf:Description>
			</rdf:RDF>
		</div>
		<rdf:RDF>
			<rdf:Description rdf:about="http://example.com/ns#r3">
				<ex:test rdf:resource="http://example.com/ns#test" />
				<ex:literal>Foo</ex:literal>
			</rdf:Description>
		</rdf:RDF>
	</body>
</html>
EOF

my $parser = RDF::RDFa::Parser->new($xhtml, 'http://example.com/', {embedded_rdfxml=>1});
$parser->consume;

my $model;
ok($model = $parser->graph, "Graph retrieved");

ok(!$model->count_statements(
		RDF::Trine::Node::Resource->new('http://example.com/ns#r2'),
		RDF::Trine::Node::Resource->new('http://example.com/ns#test'),
		RDF::Trine::Node::Resource->new('http://example.com/ns#test'),
		),
	"RDF/XML inside rdf:XMLLiterals is ignored.");

ok($model->count_statements(
		RDF::Trine::Node::Resource->new('http://example.com/ns#r3'),
		RDF::Trine::Node::Resource->new('http://example.com/ns#test'),
		RDF::Trine::Node::Resource->new('http://example.com/ns#test'),
		),
	"Embedded RDF/XML is parsed.");

ok($model->count_statements(
		RDF::Trine::Node::Resource->new('http://example.com/ns#r3'),
		RDF::Trine::Node::Resource->new('http://example.com/ns#literal'),
		RDF::Trine::Node::Literal->new('Foo', 'en-gb'),
		),
	"Language tags correctly inherited.");

$parser = RDF::RDFa::Parser->new($xhtml, 'http://example.com/', {embedded_rdfxml=>2});
$parser->consume;

ok($model = $parser->graph, "Alternative graph retrieved");

ok(!$model->count_statements(
		RDF::Trine::Node::Resource->new('http://example.com/ns#r3'),
		RDF::Trine::Node::Resource->new('http://example.com/ns#test'),
		RDF::Trine::Node::Resource->new('http://example.com/ns#test'),
		),
	"Embedded RDF/XML is skipped.");

$parser = RDF::RDFa::Parser->new($xhtml, 'http://example.com/', {embedded_rdfxml=>1});
$parser->named_graphs('http://example.com/graphing', 'graph');
$parser->consume;

$model = $parser->graph;

ok($model->count_statements(
		RDF::Trine::Node::Resource->new('http://example.com/ns#r3'),
		RDF::Trine::Node::Resource->new('http://example.com/ns#literal'),
		RDF::Trine::Node::Literal->new('Foo', 'en-gb'),
		RDF::Trine::Node::Blank->new('RDFaAutoNode000'),
		),
	"Named graphs work with RDF/XML.");
