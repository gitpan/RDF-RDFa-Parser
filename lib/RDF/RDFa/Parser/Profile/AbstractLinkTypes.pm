package RDF::RDFa::Parser::Profile::AbstractLinkTypes;

use base qw(RDF::RDFa::Parser::Profile);
use common::sense;
use 5.008;

our $VERSION = '1.093';
our %Positions;

sub new
{
	my ($class, $uri, $parser) = @_;

	my @DATA = $class->DATA;
	
	my ($profile_uri) = split /\s/, (shift @DATA);
	my ($prefix)      = split /\s/, (shift @DATA);
	
	return undef
		unless lc $uri eq $profile_uri;
		
	my $self = bless [], $class;

	if ( $self->refresh_data( $parser->{'options'}->lwp_ua, $uri ) )
	{
		my @DATA = $self->DATA;
		shift @DATA;
		shift @DATA;
	}

	while ($_ = shift @DATA)
	{
		my ($term)  = split /\s/;
		next unless $term;
		push @$self, [$self->canonical_term($term), $self->canonical_uri($prefix.$term), 0, 'rel rev'];
		push @$self, [$self->canonical_term($term), $self->canonical_uri($prefix.$term), 1, 'rel rev']
			if $self->provide_case_insensitive($term);
	}
	
	return $self;
}

sub refresh_data
{
	return;
}

sub canonical_term
{
	my ($self, $term) = @_;
	return $term;
}

sub canonical_uri
{
	my ($self, $uri) = @_;
	return $uri;
}

sub provide_case_insensitive
{
	return 1;
}

sub get_terms
{
	my $self = shift;
	return @$self;
}

sub DATA
{
	return split /\r?\n/, <<'DATA';
tag:buzzword.org.uk,2010:rdfa:profile:abstract-link-types
tag:buzzword.org.uk,2010:rdfa:profile:abstract-link-types:
test-link-type
DATA
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::AbstractLinkTypes - Abstract Link Type Registry

=head1 DESCRIPTION

Abstract class used by IETFLinkTypes, HTML4LinkTypes, HTML5LinkTypes and
HTML32LinkTypes.

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
