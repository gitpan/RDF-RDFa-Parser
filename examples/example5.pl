#!/usr/bin/perl

use lib "lib";
use RDF::RDFa::Parser;

my $parser = RDF::RDFa::Parser->new_from_url('http://uk.rottentomatoes.com/m/1198124-shutter_island/');

foreach my $property (sort $parser->opengraph)
{
	print "$property :\n";
	foreach my $val (sort $parser->opengraph($property))
	{
		print "  * $val\n";
	}
}
