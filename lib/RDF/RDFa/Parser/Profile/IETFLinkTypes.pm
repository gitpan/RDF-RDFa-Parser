package RDF::RDFa::Parser::Profile::IETFLinkTypes;

use base qw(RDF::RDFa::Parser::Profile::AbstractLinkTypes);
use common::sense;
use 5.008;

our $VERSION = '1.092';

sub DATA
{
	return split /\r?\n/, <<'DATA';
tag:buzzword.org.uk,2010:rdfa:profile:ietf	# profile URI
http://www.iana.org/assignments/relation/	# URI prefix 
alternate
appendix
bookmark
chapter
contents
copyright
current
describedby
edit
edit-media
enclosure
first
glossary
help
hub
index
last
latest-version
license
next
next-archive
payment
predecessor-version
prev
previous
prev-archive
related
replies
section
self
service
start
stylesheet
subsection
successor-version
up
version-history
via
working-copy
working-copy-of
DATA
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::IETFLinkTypes - IETF Link Type Registry

=head1 DESCRIPTION

Hard-coded profile for IETF Link Type Registry

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
