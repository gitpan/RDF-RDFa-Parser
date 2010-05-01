package RDF::RDFa::Parser::Profile::PrefixCC;

use File::Spec;
use HTTP::Cache::Transparent;
use LWP::UserAgent;

use base qw(RDF::RDFa::Parser::Profile);
use strict;
use 5.008;

our $VERSION = '1.09_05';

BEGIN
{
	HTTP::Cache::Transparent::init({BasePath => File::Spec->tmpdir.'/cache/'});
	
	# This lets RDF::RDFa::Parser find this module.
	push @RDF::RDFa::Parser::Profile::Modules, __PACKAGE__;
}

sub new
{
	my ($class, $uri, $parser) = @_;
	
	return undef
		unless $uri =~ m'^http://prefix.cc/(([A-Z0-9-]+)(,[A-Z0-9-]+)*)'i;
	
	my $prefixes = $1;
	my $self = bless [], $class;
	
	my $known = {};
	while (<DATA>)
	{
		chomp;
		my ($p, $u)  = split /\t/;
		$known->{$p} = $u;
	}
	
	my @prefixes = split /\,/, $prefixes;
	my @still_unknown;
	foreach my $p (@prefixes)
	{
		if (defined $known->{lc $p})
		{
			push @$self, [lc $p, $known->{$p}];
		}
		else
		{
			push @still_unknown, $p;
		}
	}

	if (@still_unknown)
	{
		my $ua       = LWP::UserAgent->new( agent => __PACKAGE__.' ' );
		my $response = $ua->get(
			sprintf(
				'http://prefix.cc/%s.txt.plain',
				(join ',', (sort @still_unknown))
				)
			);
		
		if ($response->code == 200)
		{
			my @line = split /\r?\n/, $response->decoded_content;
			foreach (@line)
			{
				my @fields = split /\s+/;
				push @$self, \@fields;
			}
		}
	}
	
	return $self if @$self;
	return undef;
}

sub get_prefixes
{
	my $self = shift;
	return @$self;
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::PrefixCC - use prefix.cc URIs as profiles

=head1 DESCRIPTION

Allows URIs like L<http://prefix.cc/dc,foaf,rdfs,sioc> to be used as RDFa profiles.
This module has twenty commonly used prefixed hardcoded to avoid actual HTTP 
lookups against prefix.cc.

This is primarily a demonstration of how RDF::RDFa::Parser can be extended using
alternative Profile modules.

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