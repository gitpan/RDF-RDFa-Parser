package RDF::RDFa::Parser::Profile::HTML32LinkTypes;

use base qw(RDF::RDFa::Parser::Profile::AbstractLinkTypes);
use common::sense;
use 5.008;

our $VERSION = '1.094';

sub DATA
{
	return split /\r?\n/, <<'DATA';
tag:buzzword.org.uk,2010:rdfa:profile:html32	# profile URI
http://www.w3.org/1999/xhtml/vocab#	# URI prefix 
top
contents
index
glossary
copyright
next
previous
help
search
chapter
made
DATA
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::HTML32LinkTypes

=head1 DESCRIPTION

Hard-coded profile for HTML 3.2 Link Types.

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
