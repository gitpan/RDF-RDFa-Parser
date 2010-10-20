package RDF::RDFa::Parser::Profile::ARIARoles;

use base qw(RDF::RDFa::Parser::Profile::AbstractRoles);
use common::sense;
use 5.008;

our $VERSION = '1.092';

sub DATA
{
	return split /\r?\n/, <<'DATA';
tag:buzzword.org.uk,2010:rdfa:profile:aria-role	# profile URI
http://www.w3.org/1999/xhtml/vocab#	# URI prefix 
alert
alertdialog
application
article
button
checkbox
columnheader
combobox
dialog
directory
document
grid
gridcell
group
heading
img
link
list
listbox
listitem
log
marquee
math
menu
menubar
menuitem
menuitemcheckbox
menuitemradio
option
presentation
progressbar
radio
radiogroup
region
row
rowheader
separator
slider
spinbutton
status
tab
tablist
tabpanel
textbox
timer
toolbar
tooltip
tree
treegrid
treeitem
DATA
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::ARIARoles - ARIA Roles Collection

=head1 DESCRIPTION

Hard-coded profile for ARIA roles.

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
