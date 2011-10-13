package RDF::RDFa::Parser::Profile::SearchMonkey;

use base qw(RDF::RDFa::Parser::Profile);
use common::sense;
use 5.010;

our $VERSION = '1.096';

sub new
{
	my ($class, $uri, $parser) = @_;
	
	return undef
		unless $uri eq 'http://search.yahoo.com/searchmonkey-profile';
	
	my $self = bless [], $class;
	
	my @DATA = &DATA;
	while ($_ = shift @DATA)
	{
		chomp;
		my ($p, undef, $u)  = split /\t/;
		push @$self, [$p, $u];
	}	
	
	return $self;
}

sub get_prefixes
{
	my $self = shift;
	return @$self;
}

sub DATA
{
	# copied and pasted from Yahoo documentation, 2010-05-26.
	return split /\r?\n/, <<'DATA';
abmeta	AB Meta	http://www.abmeta.org/ns#
action	SearchMonkey Actions	http://search.yahoo.com/searchmonkey/action/
assert	SearchMonkey Assertions (deprecated)	http://search.yahoo.com/searchmonkey/assert/
cc	Creative Commons	http://creativecommons.org/ns#
commerce	SearchMonkey Commerce	http://search.yahoo.com/searchmonkey/commerce/
context	SearchMonkey Context (deprecated)	http://search.yahoo.com/searchmonkey/context/
country	SearchMonkey Country Datatypes	http://search.yahoo.com/searchmonkey-datatype/country/
currency	SearchMonkey Currency Datatypes	http://search.yahoo.com/searchmonkey-datatype/currency/
dbpedia	DBPedia	http://dbpedia.org/resource/
dc	Dublin Core	http://purl.org/dc/terms/
fb	Freebase	http://rdf.freebase.com/
feed	SearchMonkey Feed	http://search.yahoo.com/searchmonkey/feed/
finance	SearchMonkey Finance	http://search.yahoo.com/searchmonkey/finance/
foaf	FOAF	http://xmlns.com/foaf/0.1/
geo	GeoRSS	http://www.georss.org/georss#
gr	GoodRelations	http://purl.org/goodrelations/v1#
job	SearchMonkey Jobs	http://search.yahoo.com/searchmonkey/job/
media	SearchMonkey Media	http://search.yahoo.com/searchmonkey/media/
news	SearchMonkey News	http://search.yahoo.com/searchmonkey/news/
owl	OWL ontology language	http://www.w3.org/2002/07/owl#
page	SearchMonkey Page (deprecated)	http://search.yahoo.com/searchmonkey/page/
product	SearchMonkey Product	http://search.yahoo.com/searchmonkey/product/
rdf	RDF	http://www.w3.org/1999/02/22-rdf-syntax-ns#
rdfs	RDF Schema	http://www.w3.org/2000/01/rdf-schema#
reference	SearchMonkey Reference	http://search.yahoo.com/searchmonkey/reference/
rel	SearchMonkey Relations	http://search.yahoo.com/searchmonkey-relation/
resume	SearchMonkey Resume	http://search.yahoo.com/searchmonkey/resume/
review	Review	http://purl.org/stuff/rev#
sioc	SIOC	http://rdfs.org/sioc/ns#
social	SearchMonkey Social	http://search.yahoo.com/searchmonkey/social/
stag	Semantic Tags	http://semantictagging.org/ns#
tagspace	SearchMonkey Tagspace (deprecated)	http://search.yahoo.com/searchmonkey/tagspace/
umbel	UMBEL	http://umbel.org/umbel/sc/
use	SearchMonkey Use Datatypes	http://search.yahoo.com/searchmonkey-datatype/use/
vcal	VCalendar	http://www.w3.org/2002/12/cal/icaltzd#
vcard	VCard	http://www.w3.org/2006/vcard/ns#
xfn	XFN	http://gmpg.org/xfn/11#
xhtml	XHTML	http://www.w3.org/1999/xhtml/vocab#
xsd	XML Schema Datatypes	http://www.w3.org/2001/XMLSchema#
DATA
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::SearchMonkey - Yahoo SearchMonkey profile

=head1 DESCRIPTION

URI: http://search.yahoo.com/searchmonkey-profile

=head1 SEE ALSO

L<RDF::RDFa::Parser>,
L<RDF::RDFa::Parser::Profile>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT

Copyright 2008-2011 Toby Inkster

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
