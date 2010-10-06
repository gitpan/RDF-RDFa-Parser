package RDF::RDFa::Parser::Profile;

use strict;
use 5.008;

use RDF::RDFa::Parser::Profile::Null;
use RDF::RDFa::Parser::Profile::PrefixCC;
use RDF::RDFa::Parser::Profile::SearchMonkey;
use RDF::RDFa::Parser::Profile::RDF;
use Scalar::Util qw'blessed';

our @Modules;
our %Known;
our $VERSION = '1.091';

sub new
{
	my ($class, $uri, $parser) = @_;

	# Return cached result.
	return $Known{$uri}
		if blessed($Known{$uri}) && $Known{$uri}->isa(__PACKAGE__);
	return undef
		if $Known{$uri} eq '-';

	# Try non-default profile modules first.
	foreach my $m (@Modules)
	{
		next if $m eq 'use RDF::RDFa::Parser::Profile::RDF';
		my $p = $m->new($uri, $parser);
		if ($p)
		{
			$Known{$uri} = $p;
			return $p;
		}
	}
	
	# Fall back to ::RDF, the default.
	my $p = RDF::RDFa::Parser::Profile::RDF->new($uri, $parser);
	if ($p)
	{
		$Known{$uri} = $p;
		return $p;
	}
	
	# If that didn't work, return undef.
	$Known{$uri} = '-';
	return undef;
}

sub get_terms
{
	return qw();
}

sub get_prefixes
{
	return qw();
}

sub get_vocabulary
{
	return undef;
}

1;

__END__

=head1 NAME

RDF::RDFa::Parser::Profile - base class for RDFa profiles

=head1 DESCRIPTION

This is a base class for RDFa profiles. You don't need to know anything
about this module unless you're doing some seriously weird stuff.

The constructor (C<new>) takes a URI for an RDF profile and returns either
undef (meaning "I don't want anything to do with that profile") or an object
with C<get_terms>, C<get_prefixes> and C<get_vocabulary> methods.

C<get_terms> returns a list of arrayrefs such that each arrayref has the
following entries:

=over

=item 0. A term (keyword) defined by the profile.

=item 1. The full URI is should expand to.

=item 2. True iff the term is case-insensitive.

=item 3. A space-separated list of attributes the term is allowed in. (Defaults to '*'.)

=back

C<get_prefixes> returns a list of arrayrefs such that each arrayref has the following
entries:

=over

=item 0. A prefix defined by the profile.

=item 1. The full URI is should expand to.

=item 2. True iff the term is case-insensitive.

=back

C<get_vocabulary> returns the URI stem for the default vocabulary set by the
profile, or undef if no such vocabulary is set by the profile.

=head1 SEE ALSO

L<RDF::RDFa::Parser>,
L<RDF::RDFa::Parser::Profile::Null>,
L<RDF::RDFa::Parser::Profile::PrefixCC>,
L<RDF::RDFa::Parser::Profile::RDF>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT

Copyright 2008-2010 Toby Inkster

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
