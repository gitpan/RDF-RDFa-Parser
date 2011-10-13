#!/usr/bin/perl

use lib "lib";
use RDF::RDFa::Parser;
use RDF::TrineShortcuts;

my $uri = shift;

my $parser = RDF::RDFa::Parser->new_from_url(
	$uri,
	RDF::RDFa::Parser::Config->new('html', 'guess', foo=>2, cite_attr=>1, role_attr=>1, longdesc_attr=>1),
	);

print rdf_string($parser->graph => 'Turtle');
