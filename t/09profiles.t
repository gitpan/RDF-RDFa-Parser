use Test::More tests => 5;
BEGIN { use_ok('RDF::RDFa::Parser') };

my $testdoc = <<'TESTDOC';
<html xmlns="http://www.w3.org/1999/xhtml">
	<head>
		<title>Test Document</title>
	</head>
	<body>
		<div about="#x1" typeof="FOOBAR"></div>
		<div about="#x2" typeof="FOOBAR" profile="http://example.com/profile"></div>
		<div about="#x3" typeof="ex:Quux" profile="http://example.com/profile"></div>
		<div about="#x4" typeof="Xyzzy" profile="http://example.com/profile"></div>
	</body>
</html>
TESTDOC

push @RDF::RDFa::Parser::Profile::Modules, 'RDF::RDFa::Parser::Profile::JustTesting';

my $opts = RDF::RDFa::Parser::Config->new('xhtml','1.1');
my $p    = RDF::RDFa::Parser->new($testdoc, "http://example.net/document.xhtml", $opts);

#my $profile = RDF::RDFa::Parser::Profile::JustTesting->new("http://example.com/profile", $p);
#use Data::Dumper;
#diag(Dumper($profile));

my $graph = $p->graph;

ok(
	!$graph->count_statements(
		RDF::Trine::Node::Resource->new('http://example.net/document.xhtml#x1'),
		RDF::Trine::Node::Resource->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
		undef
		),
	"typeof=undefined does not generate a triple"
	);

ok(
	$graph->count_statements(
		RDF::Trine::Node::Resource->new('http://example.net/document.xhtml#x2'),
		RDF::Trine::Node::Resource->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
		RDF::Trine::Node::Resource->new('http://example.com/ns#Foobar'),
		),
	"Term mapping works"
	);

ok(
	$graph->count_statements(
		RDF::Trine::Node::Resource->new('http://example.net/document.xhtml#x3'),
		RDF::Trine::Node::Resource->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
		RDF::Trine::Node::Resource->new('http://example.com/ns#Quux'),
		),
	"Prefix mapping works"
	);

ok(
	$graph->count_statements(
		RDF::Trine::Node::Resource->new('http://example.net/document.xhtml#x4'),
		RDF::Trine::Node::Resource->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
		RDF::Trine::Node::Resource->new('http://example.com/vocab#Xyzzy'),
		),
	"Vocab definition works"
	);

package RDF::RDFa::Parser::Profile::JustTesting;

use base 'RDF::RDFa::Parser::Profile::RDF';
use HTTP::Response;

sub new
{
	my ($class, $uri, $parser) = @_;
	
	return undef
		unless $uri eq 'http://example.com/profile';
	
	my $response = HTTP::Response->new('200', 'OK', [
		'content-base' => $uri,
		'content-type' => 'text/turtle',
	], <<'PROFILE');
@prefix rdfa: <http://www.w3.org/ns/rdfa#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

<>
	a rdfa:ProfileDocument ;
	rdfa:defines [
		a rdfa:PrefixMapping ;
		rdfa:prefix "ex"^^xsd:string ;
		rdfa:uri "http://example.com/ns#"^^xsd:string 
	] ;
	rdfa:defines [
		a rdfa:TermMapping ;
		rdfa:term "FOOBAR"^^xsd:string ;
		rdfa:uri "http://example.com/ns#Foobar"^^xsd:anyURI
	] ;
	rdfa:vocabulary "http://example.com/vocab#"^^xsd:string .
PROFILE
		
	return $class->new_from_response($response, $parser);
}

1;
