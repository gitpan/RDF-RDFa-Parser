package RDF::RDFa::Parser::Profile::GRDDLLinkTypes;

use base qw(RDF::RDFa::Parser::Profile::AbstractLinkTypes);
use common::sense;
use 5.010;

our $VERSION = '1.096';

sub DATA
{
	return split /\r?\n/, <<'DATA';
http://www.w3.org/2003/g/data-view	# profile URI
http://www.w3.org/2003/g/data-view#	# URI prefix 
transformation
profileTransformation
namespaceTransformation
DATA
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::GRDDLLinkTypes - GRDDL Link Types

=head1 DESCRIPTION

URI: http://www.w3.org/2003/g/data-view.

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
