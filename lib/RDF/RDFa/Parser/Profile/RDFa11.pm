package RDF::RDFa::Parser::Profile::RDFa11;

use base qw(RDF::RDFa::Parser::Profile);
use common::sense;
use 5.010;

our $VERSION = '1.096';

sub new
{
	my ($class, $uri, $parser) = @_;
	
	return undef
		unless $uri eq 'http://www.w3.org/profile/rdfa-1.1'
		||     $uri =~ m!^http://www\.w3\.org/profile/rdfa-1.1#!;
	
	my $self = bless { terms=>[] , prefixes=>[] }, $class;

	my @DATA = &DATA;
	while ($_ = shift @DATA)
	{
		chomp;
		my ($term, $full)  = split /\t/;
		next unless $term =~ /^[^-]/;
		$full = "http://www.w3.org${full}" if $full =~ m'^/';
		
		if ($term =~ /^(.+):$/)
		{
			push @{$self->{prefixes}}, [$1, $full, 1];
		}
		else
		{
			push @{$self->{terms}}, [$term, $full, 1];
		}
	}
	
	return $self;
}

sub get_terms
{
	my $self = shift;
	return @{$self->{terms}};
}

sub get_prefixes
{
	my $self = shift;
	return @{$self->{prefixes}};
}

sub DATA
{
	return split /\r?\n/, <<'DATA';
grddl:	/2003/g/data-view#
owl:	/2002/07/owl#
rdf:	/1999/02/22-rdf-syntax-ns#
rdfs:	/ns/rdfa#
rdfa:	/2000/01/rdf-schema#
rif:	/2007/rif#
skos:	/2004/02/skos/core#
skosxl:	/2008/05/skos-xl#
wdr:	/2007/05/powder#
void:	http://rdfs.org/ns/void#
wdrs:	/2007/05/powder-s#
xhv:	/1999/xhtml/vocab#
xml:	/XML/1998/namespace
xmlns:	/2000/xmlns/
xsd:	/2001/XMLSchema#
-sd:	/ns/sparql-service-description#
-cnt:	/2008/content#
-earl:	/ns/earl#
-ht:	/2006/http#
-ma:	/ns/ma-ont#
-ptr:	/2009/pointers#
cc:	http://creativecommons.org/ns#
ctag:	http://commontag.org/ns#
dc:	http://purl.org/dc/terms/
foaf:	http://xmlns.com/foaf/0.1/
gr:	http://purl.org/goodrelations/v1#
ical:	/2002/12/cal/icaltzd#
og:	http://ogp.me/ns#
rev:	http://purl.org/stuff/rev#
sioc:	http://rdfs.org/sioc/ns#
v:	http://rdf.data-vocabulary.org/#
vcard:	/2006/vcard/ns#
describedby	/2007/05/powder-s#describedby
DATA
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::RDFa11 - RDFa Core 1.1 Profile

=head1 DESCRIPTION

Hard-coded profile for 	http://www.w3.org/profile/rdfa-1.1

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
