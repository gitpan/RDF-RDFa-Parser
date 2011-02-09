package RDF::RDFa::Parser::Profile::IETFLinkTypes;

use base qw(RDF::RDFa::Parser::Profile::AbstractLinkTypes);
use common::sense;
use constant
	REGISTRY_URL => 'http://www.iana.org/assignments/link-relations/link-relations.xml';
use 5.008;

use File::Spec;
use XML::LibXML;

our $cache_path = File::Spec->catfile(
	File::Spec->tmpdir,
	'RDF-RDFa-Parser',
	'link-relations.xml',
	);

our $VERSION = '1.094';

my $hard_coded = <<'DATA';
tag:buzzword.org.uk,2010:rdfa:profile:ietf	# profile URI
http://www.iana.org/assignments/relation/	# URI prefix 
alternate
appendix
archives
author
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
icon
index
last
latest-version
license
monitor
monitor-group
next
next-archive
nofollow
noreferrer
payment
predecessor-version
prefetch
prev
previous
prev-archive
related
replies
search
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

my $loaded = undef;

sub refresh_data
{
	my ($self, $ua, $uri) = @_;

	return undef if $loaded;
	
	unless (-e $cache_path and -M $cache_path < 60*60*24)
	{
		$ua->mirror(REGISTRY_URL, $cache_path);
		my $atime = my $mtime = time;
		utime $atime, $mtime, $cache_path;
	}
	
	if (-e $cache_path)
	{
		my $registry = XML::LibXML->new->parse_file($cache_path);
		if ($registry)
		{
			$loaded = "tag:buzzword.org.uk,2010:rdfa:profile:ietf	# profile URI\n"
				. "http://www.iana.org/assignments/relation/	# URI prefix\n";
			my $linktypes = $registry
				->documentElement
				->getChildrenByTagName('registry')
				->shift
				->getChildrenByTagName('record');
			while (my $linktype = $linktypes->shift)
			{
				$loaded .= $linktype
					->getChildrenByTagName('value')
					->shift
					->textContent;
				$loaded .= "\n";
			}
			return 1; # &new should reload data
		}
	}
	
	return;
}

sub DATA
{
	return split /\r?\n/, ($loaded or $hard_coded);
}

1;

=head1 NAME

RDF::RDFa::Parser::Profile::IETFLinkTypes - IETF Link Type Registry

=head1 DESCRIPTION

Cached profile for IETF Link Type Registry, with hard-coded backup.

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
