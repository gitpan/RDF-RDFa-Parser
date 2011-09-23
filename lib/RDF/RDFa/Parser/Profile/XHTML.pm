package RDF::RDFa::Parser::Profile::XHTML;

use base qw(RDF::RDFa::Parser::Profile);
use common::sense;
use 5.008;

our $VERSION = '1.095';

sub new
{
	my ($class, $uri, $parser) = @_;
	
	return undef
		unless $uri eq 'http://www.w3.org/profile/html-rdfa-1.1'
		||     $uri =~ m!^http://www\.w3\.org/profile/html-rdfa-1\.1#!;
	
	my $self = bless [], $class;

	my @DATA = &DATA;
	while ($_ = shift @DATA)
	{
		chomp;
		my ($term, $full)  = split /\s/;
		next unless $term;
		
		$full = 'http://www.w3.org/1999/xhtml/vocab#'.$term unless $full;
		$full = "http://www.w3.org${full}" if $full =~ m'^/';
		
		push @$self, [$term, $full, 0, '*'];
		push @$self, [$term, $full, 1, '*'];
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
itsRules
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
transaction	/2003/g/data-view#transformation
up
DATA
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::XHTML - XHTML+RDFa 1.1 Profile

=head1 DESCRIPTION

Default profile for (X)HTML+RDFa 1.1.

URI: http://www.w3.org/profile/html-rdfa-1.1

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
