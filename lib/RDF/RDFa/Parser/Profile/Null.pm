package RDF::RDFa::Parser::Profile::Null;

use base qw(RDF::RDFa::Parser::Profile);
use strict;
use 5.008;

our $VERSION = '1.09_07';

BEGIN
{
	# This lets RDF::RDFa::Parser find this module.
	push @RDF::RDFa::Parser::Profile::Modules, __PACKAGE__;
}

sub new
{
	my ($class, $uri, $parser) = @_;
	my $self = bless {}, $class;
	
	return $self
		if $uri =~ m'^http://microformats.org/profile/'i
		|| $uri =~ m'^http://ufs.cc/x/'i
		|| $uri =~ m'^http://gmpg.org/xfn/1'i
		|| $uri =~ m'^http://purl.org/uF/'i
		|| $uri eq 'http://dublincore.org/documents/2008/08/04/dc-html/'
		|| $uri eq 'http://purl.org/NET/erdf/profile'
		|| $uri eq 'http://www.w3.org/1999/xhtml/vocab'
		|| $uri eq 'http://www.w3.org/2005/10/profile'
		;
	
	return undef;
}


1;

=head1 NAME

RDF::RDFa::Parser::Profile::Null - hard-coded null profiles

=head1 DESCRIPTION

Certain commonly-used profile URIs are known not to contain RDFa
profiles. This module hard-codes them as null profiles to avoid
performing useless HTTP requests.

=head1 SEE ALSO

L<RDF::RDFa::Parser>,
L<RDF::RDFa::Parser::Profile>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT

Copyright 2008-2010 Toby Inkster

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

__DATA__
cc	http://creativecommons.org/ns#
ctag	http://commontag.org/ns#
dc	http://purl.org/dc/terms/
doap	http://usefulinc.com/ns/doap#
foaf	http://xmlns.com/foaf/0.1/
geo	http://www.w3.org/2003/01/geo/wgs84_pos#
gr	http://purl.org/goodrelations/v1#
owl	http://www.w3.org/2002/07/owl#
rdf	http://www.w3.org/1999/02/22-rdf-syntax-ns#
rdfa	http://www.w3.org/ns/rdfa#
rdfs	http://www.w3.org/2000/01/rdf-schema#
rel	http://purl.org/vocab/relationship/
rev	http://purl.org/stuff/rev#
rss	http://purl.org/rss/1.0/
sioc	http://rdfs.org/sioc/ns#
skos	http://www.w3.org/2004/02/skos/core#
tag	http://www.holygoat.co.uk/owl/redwood/0.1/tags/
xfn	http://vocab.sindice.com/xfn#
xhv	http://www.w3.org/1999/xhtml/vocab#
xsd	http://www.w3.org/2001/XMLSchema#
