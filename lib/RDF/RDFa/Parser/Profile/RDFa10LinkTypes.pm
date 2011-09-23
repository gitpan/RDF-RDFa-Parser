package RDF::RDFa::Parser::Profile::RDFa10LinkTypes;

use base qw(RDF::RDFa::Parser::Profile::AbstractLinkTypes);
use common::sense;
use 5.008;

our $VERSION = '1.095';

sub DATA
{
	return split /\r?\n/, <<'DATA';
http://www.w3.org/1999/xhtml/vocab	# profile URI
http://www.w3.org/1999/xhtml/vocab#	# URI prefix 
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

RDF::RDFa::Parser::Profile::RDFa10LinkTypes - XHTML+RDFa 1.0 Default Profile

=head1 DESCRIPTION

Default profile for XHTML+RDFa 1.0.

URI: http://www.w3.org/1999/xhtml/vocab

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
