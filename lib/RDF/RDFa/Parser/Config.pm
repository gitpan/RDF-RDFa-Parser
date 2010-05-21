package RDF::RDFa::Parser::Config;

use base qw(Exporter);
use constant {
	HOST_ATOM  => 'atom',
	HOST_HTML4 => 'html4',
	HOST_HTML5 => 'html5',
	HOST_SVG   => 'svg',
	HOST_XHTML => 'xhtml',
	HOST_XML   => 'xml',
};
use constant {
	RDFA_10    => '1.0',
	RDFA_11    => '1.1',
};
use strict;
use 5.008;

our @EXPORT_OK = qw(HOST_ATOM HOST_HTML4 HOST_HTML5 HOST_SVG HOST_XHTML HOST_XML RDFA_10 RDFA_11);
our $VERSION = '1.09_08';
our $CONFIGS = {
	'host' => {
		HOST_ATOM() => {
			'atom_elements'         => 1,
			'keyword_bundles'       => 'rdfa iana',
		},
		HOST_HTML4() => {
			'dom_parser'            => 'html',
			'embedded_rdfxml'       => 0,
			'prefix_nocase_xmlns'   => 1,
			'keyword_bundles'       => 'rdfa html4',
			'xhtml_base'            => 1,
			'xhtml_elements'        => 1,
			'xhtml_lang'            => 1,
			'xml_base'              => 0,
			'xml_lang'              => 0,
		},
		HOST_HTML5() => {
			'dom_parser'            => 'html',
			'embedded_rdfxml'       => 0,
			'prefix_nocase_xmlns'   => 1,
			'keyword_bundles'       => 'rdfa html5',
			'xhtml_base'            => 1,
			'xhtml_elements'        => 1,
			'xhtml_lang'            => 1,
			'xml_base'              => 0,
			'xml_lang'              => 1,
		},
		HOST_SVG() => {},
		HOST_XHTML() => {
			'embedded_rdfxml'       => 0,
			'keyword_bundles'       => 'rdfa',
			'xhtml_base'            => 1,
			'xhtml_elements'        => 1,
			'xml_base'              => 0,
		},
		HOST_XML() => {},
	},
	'rdfa' => {
		RDFA_10() => {
			'alt_stylesheet'        => 0,
			'atom_elements'         => 0,
			'atom_parser'           => 0,
			'auto_config'           => 0,
			'dom_parser'            => 'xml',
			'embedded_rdfxml'       => 1,
			'full_uris'             => 0,
			'keyword_bundles'       => 'rdfa',
			'graph'                 => 0,
			'graph_attr'            => 'graph',
			'graph_type'            => 'id',
			'graph_default'         => '_:RDFaDefaultGraph',
			'graph_default_trine'   => undef,  # not officially exposed
			'prefix_attr'           => 0,
			'prefix_bare'           => 0,
			'prefix_default'        => 0,
			'prefix_empty'          => 'http://www.w3.org/1999/xhtml/vocab#',
			'prefix_nocase_attr'    => 0,
			'prefix_nocase_xmlns'   => 0,
			'profiles'              => 0,
			'role_attr'             => 0,
			'safe_anywhere'         => 0,
			'tdb_service'           => 0,
			'use_rtnlx'             => 0,
			'vocab_attr'            => 0,
			'xhtml_base'            => 0,
			'xhtml_elements'        => 0,
			'xhtml_lang'            => 0,
			'xml_base'              => 2,
			'xml_lang'              => 1,
			'xmlns_attr'            => 1,
		},
		RDFA_11() => {
			'alt_stylesheet'        => 0,
			'atom_elements'         => 0,
			'atom_parser'           => 0,
			'auto_config'           => 0,
			'dom_parser'            => 'xml',
			'embedded_rdfxml'       => 1,
			'full_uris'             => 1, #diff
			'keyword_bundles'       => '', #diff
			'graph'                 => 0,
			'graph_attr'            => 'graph',
			'graph_type'            => 'id',
			'graph_default'         => '_:RDFaDefaultGraph',
			'graph_default_trine'   => undef,
			'prefix_attr'           => 1, #diff
			'prefix_bare'           => 0,
			'prefix_default'        => 0,
			'prefix_empty'          => 'http://www.w3.org/1999/xhtml/vocab#',
			'prefix_nocase_attr'    => 1, #diff
			'prefix_nocase_xmlns'   => 1, #diff
			'profiles'              => 1, #diff
			'role_attr'             => 0,
			'safe_anywhere'         => 1, #diff
			'tdb_service'           => 0,
			'use_rtnlx'             => 0,
			'vocab_attr'            => 1, #diff
			'xhtml_base'            => 0,
			'xhtml_elements'        => 0,
			'xhtml_lang'            => 0,
			'xml_base'              => 2,
			'xml_lang'              => 1,
			'xmlns_attr'            => 1,
		},
	},
};

sub new
{
	my ($class, $host, $version, %options) = @_;
	$host    ||= HOST_XHTML;
	$version ||= RDFA_10;
	
	my $self = bless {}, $class;
	
	$self->_merge_options($CONFIGS->{'rdfa'}->{$version})
		if defined $CONFIGS->{'rdfa'}->{$version};
	
	$self->_merge_options($CONFIGS->{'host'}->{$host})
		if defined $CONFIGS->{'host'}->{$host};

	# XHTML+RDFa 1.1 wants to use @lang, though
	# neither XHTML's host language rules, nor
	# RDFa 1.1's rules individually use it.
	if ($host eq HOST_XHTML && $version eq RDFA_11)
	{
		$self->{xhtml_lang} = 1;
	}
	
	$self->_merge_options(\%options)
		if %options;
	
	$self->_expand_keywords;
	
	$self->{'_host'} = $host;
	$self->{'_rdfa'} = $version;
	$self->{'_opts'} = \%options;
	
	return $self;
}

sub rehost
{
	my ($self, $host, $version) = @_;
	$version ||= $self->{'_rdfa'};
	my $opts   = $self->{'_opts'};
	my $class  = __PACKAGE__;
	return $class->new($host, $version, %$opts);
}

sub merge_options
{
	my ($self, %opts) = @_;
	$self->_merge_options(\%opts);
	$self->_expand_keywords;
}

sub auto_config
{
	my ($self, $dom) = @_;
	my $count;
	
	return undef unless $self->{'auto_config'};

	my $xpc = XML::LibXML::XPathContext->new;
	$xpc->registerNs('x', 'http://www.w3.org/1999/xhtml');
	my $nodes  = $xpc->find('//x:meta[@name="http://search.cpan.org/dist/RDF-RDFa-Parser/#auto_config"]/@content', $dom->documentElement);
	my $optstr = '';
	foreach my $node ($nodes->get_nodelist)
	{
		$optstr .= '&' . $node->getValue;
	}
	$optstr =~ s/^\&//;
	my $options = _parse_application_x_www_form_urlencoded($optstr);
	
	my $x = {};
	
	foreach my $o (keys %$options)
	{
		next unless $o=~ /^(alt_stylesheet | embedded_rdfxml | full_uris |
			keywords | prefix_attr | prefix_bare | prefix_empty | prefix_nocase |
			safe_anywhere | tdb_service | xhtml_base | xhtml_elements | xhtml_lang |
			xml_base | xml_lang | graph | graph_attr | graph_type | graph_default |
			prefix_default | vocab_attr | profiles | keyword_bundles )$/ix;
		$count++;
		if (lc $o eq 'keywords')
		{
			$x->{keyword_bundles} .= ' ' . $options->{$o};
		}
		else
		{
			$x->{lc $o} = $options->{$o};
		}
	}
	
	$self->merge_options(%$x);
	
	return $count;
}

sub _parse_application_x_www_form_urlencoded
{
	my $axwfue = shift;
	$axwfue =~ tr/;/&/;
	$axwfue =~ s/(^&+|&+$)//g;
	my $rv = {};
	for (split /&/, $axwfue)
	{
		my ($k, $v) = split /=/, $_, 2;
		next unless length $k;
		$rv->{uri_unescape($k)} = uri_unescape($v);
	}
	return $rv;
}

sub _merge_options
{
	my ($self, $opts) = @_;
	
	while (my ($key, $value) = each %$opts)
	{
		if ($key eq 'keyword_bundles'
		&&  defined $self->{$key}
		&&  length $self->{$key})
		{
			$self->{$key} .= " $value"; 
		}
		elsif ($key eq 'prefix_nocase')
		{
			$self->{'prefix_nocase_attr'}  = $value;
			$self->{'prefix_nocase_xmlns'} = $value;
		}
		else
		{
			$self->{$key}  = $value;
		}
	}
}

sub _expand_keywords
{
	my $self  = shift;
	my $terms = $self->{'keyword_bundles'} || '';
	
	my $KW = {
		'about'     => {},
		'resource'  => {},
		'rel'       => {},
		'rev'       => {},
		'role'      => {},
		'property'  => {},
		'datatype'  => {},
		'typeof'    => {},
		'graph'     => {},
		'*'         => {},
		};
	my $KW2 = {
		'about'     => {},
		'resource'  => {},
		'rel'       => {},
		'rev'       => {},
		'role'      => {},
		'property'  => {},
		'datatype'  => {},
		'typeof'    => {},
		'graph'     => {},
		'*'         => {},
		};

	if ($terms =~ /\b(rdfa|xhv)\b/i)
	{
		foreach my $attr (qw(rel rev))
		{
			foreach my $word (qw(alternate appendix bookmark cite
				chapter contents copyright first glossary help icon
				index last license meta next p3pv1 prev role section
				stylesheet subsection start top up))
			{
				$KW->{ $attr }->{ lc $word } = 'http://www.w3.org/1999/xhtml/vocab#' . (lc $word)
					unless defined $KW->{ $attr }->{ lc $word };
			}
		}
	}

	if ($terms =~ /\b(html5)\b/i)
	{
		foreach my $attr (qw(rel rev))
		{
			foreach my $word (qw(alternate archives author bookmark
				external feed first help icon index last license next
				nofollow noreferrer pingback prefetch prev search
				stylesheet sidebar tag up ALTERNATE-STYLESHEET))
			{
				$KW->{ $attr }->{ lc $word } = 'http://www.w3.org/1999/xhtml/vocab#' . $word
					unless defined $KW->{ $attr }->{ lc $word };
			}
		}
	}
	
	if ($terms =~ /\b(html4)\b/i)
	{
		foreach my $attr (qw(rel rev))
		{
			foreach my $word (qw(Alternate Stylesheet Start Next
				Prev Contents Index Glossary Copyright Chapter
				Section Subsection Appendix Help Bookmark))
			{
				$KW->{ $attr }->{ lc $word } = 'http://www.w3.org/1999/xhtml/vocab#' . (lc $word)
					unless defined $KW->{ $attr }->{ lc $word };
			}
		}
	}
	
	if ($terms =~ /\b(html32)\b/i)
	{
		foreach my $attr (qw(rel rev))
		{
			foreach my $word (qw(top contents index glossary copyright
				next previous help search chapter made))
			{
				$KW->{ $attr }->{ lc $word } = 'http://www.w3.org/1999/xhtml/vocab#' . $word
					unless defined $KW->{ $attr }->{ lc $word };
			}
		}
	}
	
	if ($terms =~ /\b(iana)\b/i)
	{
		foreach my $attr (qw(rel rev))
		{
			foreach my $word (qw(alternate appendix bookmark chapter
				contents copyright current describedby edit edit-media
				enclosure first glossary help hub index last latest-version
				license next next-archive payment predecessor-version
				prev previous prev-archive related replies section self
				service start stylesheet subsection successor-version
				up version-history via working-copy working-copy-of))
			{
				$KW->{ $attr }->{ lc $word } = 'http://www.iana.org/assignments/relation/' . $word
					unless defined $KW->{ $attr }->{ lc $word };
			}
		}
	}

	if ($terms =~ /\b(grddl)\b/i)
	{
		foreach my $attr (qw(rel rev))
		{
			foreach my $word (qw(transformation profileTransformation namespaceTransformation))
			{
				$KW->{ $attr }->{ lc $word } = 'http://www.w3.org/2003/g/data-view#' . $word
					unless defined $KW->{ $attr }->{ lc $word };
			}
		}
	}

	if ($terms =~ /\b(xhtml-role|xhv)\b/i)
	{
		foreach my $word (qw(banner complementary contentinfo
			definition main navigation note search))
		{
			$KW->{'role'}->{ lc $word } = 'http://www.w3.org/1999/xhtml/vocab#' . $word
				unless defined $KW->{'role'}->{ lc $word };
		}
	}

	if ($terms =~ /\b(aria-role|xhv)\b/i)
	{
		foreach my $word (qw(alert alertdialog application article
			button checkbox columnheader combobox dialog directory
			document grid gridcell group heading img link list listbox
			listitem log marquee math menu menubar menuitem
			menuitemcheckbox menuitemradio option presentation
			progressbar radio radiogroup region row rowheader separator
			slider spinbutton status tab tablist tabpanel textbox timer
			toolbar tooltip tree treegrid treeitem))
		{
			$KW->{'role'}->{ lc $word } = 'http://www.w3.org/1999/xhtml/vocab#' . $word
				unless defined $KW->{'role'}->{ lc $word };
		}
	}

#	if ($terms =~ /\b(xfn)\b/i)
#	{
#		foreach my $attr (qw(rel rev))
#		{
#			foreach my $word (qw(contact acquaintance friend
#				met co-worker colleague co-resident neighbor
#				child parent sibling spouse kin muse crush date
#				sweetheart me))
#			{
#				$KW->{ $attr }->{ lc $word } = 'http://vocab.sindice.com/xfn#' . $word . "-hyperlink"
#					unless defined $KW->{ $attr }->{ lc $word };
#			}
#		}
#	}
	
	$self->{'keywords'} = { 'insensitive' => $KW , 'sensitive' => $KW2 };
}

1;

__END__

=head1 NAME

RDF::RDFa::Parser::Config - configuration sets for RDFa parser

=head1 DESCRIPTION

The third argument to the constructor for RDF::RDFa::Parser objects is a
configuration set. This module provides such configuration sets.

Confguration sets are needed by the parser so that it knows how to handle
certain features which vary between different host languages, or different
RDFa versions.

All you need to know about is the constructor:

  $config = RDF::RDFa::Parser::Config->new($host, $version, %options);

$host is the host language. Generally you would supply one of the
following constants; the default is HOST_XHTML.

=over

=item * B<< RDF::RDFa::Parser::Config->HOST_ATOM >>

=item * B<< RDF::RDFa::Parser::Config->HOST_HTML4 >>

=item * B<< RDF::RDFa::Parser::Config->HOST_HTML5 >>

=item * B<< RDF::RDFa::Parser::Config->HOST_SVG >>

=item * B<< RDF::RDFa::Parser::Config->HOST_XHTML >>

=item * B<< RDF::RDFa::Parser::Config->HOST_XML >>

=back

$version is the RDFa version. Generally you would supply one of the
following constants; the default is RDFA_10.

=over 2

=item * B<< RDF::RDFa::Parser::Config->RDFA_10 >>

=item * B<< RDF::RDFa::Parser::Config->RDFA_11 >>

=back

%options is a hash of additional options to use which override the
defaults. While many of these are useful, they probably also reduce
conformance to the official RDFa specifications. The following
options exist; defaults for XHTML+RDFa1.0 and XHTML+RDFa1.1 are shown
in brackets.

=over 2

=item * B<alt_stylesheet> - magic rel="alternate stylesheet". [0]

=item * B<atom_elements> - process <feed> and <entry> specially. [0]

=item * B<atom_parser> - extract Atom 1.0 native semantics. [0]

=item * B<auto_config> - see section "Auto Config" [0]

=item *B<dom_parser> - parser to use to turn a markup string into a DOM. 'html' or 'xml'. ['xml']

=item * B<embedded_rdfxml> - find plain RDF/XML chunks within document. 0=no, 1=handle, 2=skip. [0]                      

=item * B<full_uris> - support full URIs in CURIE-only attributes. [0, 1]

=item * B<graph> - enable support for named graphs. [0]

=item * B<graph_attr> - attribute to use for named graphs. Use Clark Notation to specify a namespace. ['graph']

=item * B<graph_type> - graph attribute behaviour ('id' or 'about'). ['id']

=item * B<graph_default> - default graph name. ['_:RDFaDefaultGraph']

=item * B<keyword_bundles> - space-separated list of bundles of keywords ('rdfa', 'html32', 'html4', 'html5', 'xhtml-role', 'aria-role', 'iana', 'xhv') ['rdfa']

=item *B<lwp_ua> - an LWP::UserAgent to use for HTTP requests. [undef]

=item * B<prefix_attr> - support @prefix rather than just @xmlns:*. [0, 1]

=item * B<prefix_bare> - support CURIEs with no colon+suffix. [0]

=item * B<prefix_default> - URI for default prefix (e.g. rel="foo"). [undef]

=item * B<prefix_empty> - URI for empty prefix (e.g. rel=":foo"). ['http://www.w3.org/1999/xhtml/vocab#']

=item * B<prefix_nocase> - DEPRECATED - shortcut for prefix_nocase_attr and prefix_nocase_xmlns.

=item * B<prefix_nocase_attr> - ignore case-sensitivity of CURIE prefixes defined via @prefix attribute. [0, 1]

=item * B<prefix_nocase_xmlns> - ignore case-sensitivity of CURIE prefixes defined via xmlns. [0, 1]

=item * B<profiles> - support RDFa profiles. [0, 1]

=item * B<role_attr> - support for XHTML @role [0]

=item * B<safe_anywhere> - allow Safe CURIEs in @rel/@rev/etc. [0, 1]

=item * B<tdb_service> - use thing-described-by.org to name some bnodes. [0]

=item *B<user_agent> - a User-Agent header to use for HTTP requests. Ignored if lwp_ua is provided. [undef]

=item * B<use_rtnlx> - use RDF::Trine::Node::Literal::XML. 0=no, 1=if available. [0]

=item * B<vocab_attr> - support @vocab from RDFa 1.1. [0, 1]

=item * B<xhtml_base> - process <base> element. 0=no, 1=yes, 2=use it for RDF/XML too. [1]                      

=item * B<xhtml_elements> - process <head> and <body> specially. [1]

=item * B<xhtml_lang> - support @lang rather than just @xml:lang. [0]

=item * B<xml_base> - support for 'xml:base' attribute. 0=only RDF/XML; 1=except @href/@src; 2=always. [0]

=item * B<xml_lang> - Support for 'xml:lang' attribute. [1]

=item * B<xmlns_attr> - Support for 'xmlns:foo' to define CURIE prefixes. [1]

=back

=head1 EXAMPLES

The following full example parses RDFa 1.1 in an Atom document, also using
the non-default 'atom_parser' option which parses native Atom elements into
the graph too.

  use RDF::RDFa::Parser;
  
  $config = RDF::RDFa::Parser::Config->new(
    RDF::RDFa::Parser::Config->HOST_ATOM,
    RDF::RDFa::Parser::Config->RDFA_11,
    atom_parser => 1,
    );
  $parser = RDF::RDFa::Parser->new($xml, $baseuri, $config)->consume;
  $data   = $parser->graph;

The following configuration set parses XHTML+RDFa 1.1 while also parsing
any RDF/XML chunks that are embedded in the document.

  use RDF::RDFa::Parser::Config qw(HOST_XHTML RDFA_11);
  $config = RDF::RDFa::Parser::Config->new(
    HOST_XHTML, RDFA_11, embedded_rdfxml=>1);

=head1 SEE ALSO

L<RDF::RDFa::Parser>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT

Copyright 2008-2010 Toby Inkster

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
