package RDF::RDFa::Parser::Profile::HTML5LinkTypes;

use base qw(RDF::RDFa::Parser::Profile::AbstractLinkTypes);
use common::sense;
use 5.008;

our $VERSION = '1.095';

sub DATA
{
	return split /\r?\n/, <<'DATA';
tag:buzzword.org.uk,2010:rdfa:profile:html5	# profile URI
http://www.w3.org/1999/xhtml/vocab#	# URI prefix 
alternate
archives
author
bookmark
external
feed
first
help
icon
index
last
license
next
nofollow
noreferrer
pingback
prefetch
prev
search
stylesheet
sidebar
tag
up
ALTERNATE-STYLESHEET
DATA
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::HTML5LinkTypes - HTML5 Link Types

=head1 DESCRIPTION

HTML5 Link Types.

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
