package RDF::RDFa::Parser::Profile::XHTMLRoles;

use base qw(RDF::RDFa::Parser::Profile::AbstractRoles);
use common::sense;
use 5.008;

our $VERSION = '1.093';

sub DATA
{
	return split /\r?\n/, <<'DATA';
tag:buzzword.org.uk,2010:rdfa:profile:xhtml-role	# profile URI
http://www.w3.org/1999/xhtml/vocab#	# URI prefix 
banner
complementary
contentinfo
definition
main
navigation
note
search
DATA
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::XHTMLRoles - XHTML Roles Collection

=head1 DESCRIPTION

Hard-coded profile for XHTML roles.

Only useful if you've got the roles attribute enabled.

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
