package RDF::RDFa::Parser::Profile::XHTML;

use File::Spec;
use HTTP::Cache::Transparent;

use base qw(RDF::RDFa::Parser::Profile);
use common::sense;
use 5.008;

our $VERSION = '1.093';

sub new
{
	my ($class, $uri, $parser) = @_;
	
	return undef
		unless $uri eq 'http://www.w3.org/1999/xhtml/vocab'
		||     $uri =~ m!^http://www\.w3\.org/1999/xhtml/vocab#!;
	
	my $self = bless [], $class;

	my @DATA = &DATA;
	while ($_ = shift @DATA)
	{
		chomp;
		my ($term)  = split /\s/;
		next unless $term;
		push @$self, [$term, 'http://www.w3.org/1999/xhtml/vocab#'.$term, 0, '*'];
		push @$self, [$term, 'http://www.w3.org/1999/xhtml/vocab#'.$term, 1, '*'];
	}
	
	return $self;
}

sub get_terms
{
	my $self = shift;
	return @$self;
}

sub DATA
{
	return split /\r?\n/, <<'DATA';
alternate
appendix
bookmark
cite
chapter
contents
copyright
first
glossary
help
icon
index
last
license
meta
next
p3pv1
prev
role
section
stylesheet
subsection
start
top
up
DATA
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::XHTML - XHTML+RDFa Profile

=head1 DESCRIPTION

Hard-coded profile for 	http://www.w3.org/1999/xhtml/vocab

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
