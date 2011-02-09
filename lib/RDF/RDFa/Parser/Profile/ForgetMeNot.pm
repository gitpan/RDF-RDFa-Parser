package RDF::RDFa::Parser::Profile::ForgetMeNot;

use File::Spec;
use HTTP::Cache::Transparent;

use base qw(RDF::RDFa::Parser::Profile);
use common::sense;
use 5.008;

our $VERSION = '1.094';

sub new
{
	my ($class, $uri, $parser) = @_;
	
	return undef
		unless $uri eq 'tag:buzzword.org.uk,2010:rdfa:profile:forgotten';
	
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
	return split /\r?\n/, <<'DATA';
dc	Dublin Core	http://purl.org/dc/terms/
foaf	FOAF	http://xmlns.com/foaf/0.1/
og	Open Graph Protocol	http://ogp.me/ns#
owl	OWL ontology language	http://www.w3.org/2002/07/owl#
rdf	RDF	http://www.w3.org/1999/02/22-rdf-syntax-ns#
rdfs	RDF Schema	http://www.w3.org/2000/01/rdf-schema#
v	Google Rich Snippets	http://rdf.data-vocabulary.org/#
xsd	XML Schema Datatypes	http://www.w3.org/2001/XMLSchema#
DATA
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::ForgetMeNot - frequently forgotten prefix declarations

=head1 SEE ALSO

L<RDF::RDFa::Parser>,
L<RDF::RDFa::Parser::Profile>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT

Copyright 2011 Toby Inkster

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
