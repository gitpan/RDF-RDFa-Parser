#!/usr/bin/perl

use 5.010;
use lib "lib";
use RDF::RDFa::Parser;

my $xhtml = <<XHTML;
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML+RDFa 1.0//EN"
                             "http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-GB" about="">
	<head profile="http://www.w3.org/1999/xhtml/vocab">
	</head>
	<body xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
		<div property=":explicitXmlLiteral" datatype="rdf:XMLLiteral">
			<span property=":explicitXmlLiteralInternal">foo</span>
		</div>
		<div property=":implicitXmlLiteral">
			<span property=":implicitXmlLiteralInternal">foo</span>
		</div>
	</body>
</html>
XHTML

my $parser = RDF::RDFa::Parser->new(
	$xhtml, 'http://example.com/', RDF::RDFa::Parser::Config->new('xhtml','1.1', role_attr=>1, cite_attr=>1));
$parser->set_callbacks({
	ontriple => sub {
		my ($self, $elems, $statement) = @_;
		while (my ($k,$v) = each %$elems)
		{
			print "# $k => ".$v->tagName . "\n" if defined $v;
		}
		print join ' ', map { $_->as_ntriples } $statement->nodes;
		print " .\n\n";
	}
	});
$parser->consume;

print "#################################\n\n";

my $parser = RDF::RDFa::Parser->new(
	$xhtml, 'http://example.com/', RDF::RDFa::Parser::Config->new('xhtml','1.0', role_attr=>1, cite_attr=>1));
$parser->set_callbacks({
	ontriple => sub {
		my ($self, $elems, $statement) = @_;
		while (my ($k,$v) = each %$elems)
		{
			print "# $k => ".$v->tagName . "\n" if defined $v;
		}
		print join ' ', map { $_->as_ntriples } $statement->nodes;
		print " .\n\n";
	}
	});
$parser->consume;

