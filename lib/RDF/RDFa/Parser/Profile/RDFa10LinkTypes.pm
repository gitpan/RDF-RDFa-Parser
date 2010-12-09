package RDF::RDFa::Parser::Profile::RDFa10LinkTypes;

use base qw(RDF::RDFa::Parser::Profile::AbstractLinkTypes);
use common::sense;
use 5.008;

our $VERSION = '1.093';

sub DATA
{
	return split /\r?\n/, <<'DATA';
tag:buzzword.org.uk,2010:rdfa:profile:rdfa10	# profile URI
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

RDF::RDFa::Parser::Profile::RDFa10LinkTypes - RDFa 1.0 Link Types

=head1 DESCRIPTION

Hard-coded profile for RDFa 1.0 Link Types.

This differs from the http://www.w3.org/1999/xhtml/vocab profile slightly
in that it only sets tokens for @rel and @rev.

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
