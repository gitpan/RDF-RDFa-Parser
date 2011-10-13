package RDF::RDFa::Parser::Profile::Null;

use base qw(RDF::RDFa::Parser::Profile);
use common::sense;
use 5.010;

our $VERSION = '1.096';

sub new
{
	my ($class, $uri, $parser) = @_;
	my $self = bless {}, $class;
	
	return $self
		if $uri =~ m'^http://microformats.org/profile/'i
		|| $uri =~ m'^http://ufs.cc/x/'i
		|| $uri =~ m'^http://gmpg.org/'i
		|| $uri =~ m'^http://purl.org/uF/'i
		|| $uri eq 'http://dublincore.org/documents/2008/08/04/dc-html/'
		|| $uri eq 'http://purl.org/NET/erdf/profile'
		|| $uri eq 'http://www.w3.org/2005/10/profile'
		;
	
	return undef;
}


1;

=head1 NAME

RDF::RDFa::Parser::Profile::Null - ignorable profiles

=head1 DESCRIPTION

Certain commonly-used profile URIs are known to not contain RDFa
profiles. This module hard-codes them as null profiles to avoid
performing useless HTTP requests.

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
