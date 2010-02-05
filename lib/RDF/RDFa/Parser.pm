#!/usr/bin/perl

=head1 NAME

RDF::RDFa::Parser - flexible RDFa parser

=head1 SYNOPSIS

 use RDF::RDFa::Parser;
 
 $parser = RDF::RDFa::Parser->new(undef, $uri)->consume;
 $graph  = $parser->graph;

=cut

package RDF::RDFa::Parser;
use Carp;
use Encode qw(encode_utf8);
use File::ShareDir qw(dist_file);
use LWP::UserAgent;
use RDF::Trine;
use URI::Escape;
use URI::URL;
use XML::LibXML qw(:all);
use strict;
use 5.008;

=head1 VERSION

1.00_02

=cut

our $VERSION = '1.00_02';
our $HAS_AWOL;

BEGIN
{
	eval "use XML::Atom::OWL;";
	$HAS_AWOL = $@ ? 0 : 1;
}

=head1 DESCRIPTION

=head2 Constructor

=over 4

=item C<< $p = RDF::RDFa::Parser->new($xhtml, $baseuri, \%options, $storage) >>

This method creates a new RDF::RDFa::Parser object and returns it.

The $xhtml variable may contain an XHTML/XML string, or a
XML::LibXML::Document. If a string, the document is parsed using
XML::LibXML::Parser, which will throw an exception if it is not
well-formed. RDF::RDFa::Parser does not catch the exception.

The base URI is needed to resolve relative URIs found in the document.
If $xhtml is undef, then RDF::RDFa::Parser will fetch $baseuri to
obtain the document to be parsed.

Options (mostly booleans) [default in brackets]:

=over 2

=item * B<alt_stylesheet> - magic rel="alternate stylesheet". [0]

=item * B<atom_elements> - process <feed> and <entry> specially. [0]

=item * B<atom_parser> - extract Atom 1.0 native semantics. [0]

=item * B<auto_config> - see section "Auto Config" [0]

=item * B<embedded_rdfxml> - find plain RDF/XML chunks within document. 0=no, 1=handle, 2=skip. [0]                      

=item * B<full_uris> - support full URIs in CURIE-only attributes. [0]

=item * B<graph> - enable support for named graphs. [0]

=item * B<graph_attr> - attribute to use for named graphs. Use Clark Notation to specify a namespace. ['graph']

=item * B<graph_type> - graph attribute behaviour ('id' or 'about'). ['id']

=item * B<graph_default> - default graph name. ['_:RDFaDefaultGraph']

=item * B<keywords> - THIS WILL VOID YOUR WARRANTY!

=item * B<prefix_attr> - support @prefix rather than just @xmlns:*. [0]

=item * B<prefix_bare> - support CURIEs with no colon+suffix. [0]

=item * B<prefix_default> - URI for default prefix (e.g. rel="foo"). [undef]

=item * B<prefix_empty> - URI for empty prefix (e.g. rel=":foo"). ['http://www.w3.org/1999/xhtml/vocab#']

=item * B<prefix_nocase> - ignore case-sensitivity of CURIE prefixes. [0]

=item * B<safe_anywhere> - allow Safe CURIEs in @rel/@rev/etc. [0] 

=item * B<tdb_service> - use thing-described-by.org to name some bnodes. [0]

=item * B<use_rtnlx> - use RDF::Trine::Node::Literal::XML. 0=no, 1=if available. [0]

=item * B<xhtml_base> - process <base> element. 0=no, 1=yes, 2=use it for RDF/XML too. [1]                      

=item * B<xhtml_elements> - process <head> and <body> specially. [1]

=item * B<xhtml_lang> - support @lang rather than just @xml:lang. [0]

=item * B<xml_base> - support for 'xml:base' attribute. 0=only RDF/XML; 1=except @href/@src; 2=always. [0]

=item * B<xml_lang> - Support for 'xml:lang' attribute. [1]

=back

The default options attempt to stick to the XHTML+RDFa spec as rigidly
as possible.

$storage is an RDF::Trine::Store object. If undef, then a new
temporary store is created.

=back

=cut

sub new
{
	my $class   = shift;
	my $xhtml   = shift;
	my $baseuri = shift;
	my $options = shift;
	my $store   = shift;
	my $DOMTree;

	unless (defined $xhtml)
	{
		my $ua = LWP::UserAgent->new;
		$ua->agent(sprintf('%s/%s ', __PACKAGE__, $VERSION));
		$ua->default_header("Accept" => "application/xhtml+xml, application/xml;q=0.1, text/xml;q=0.1");
		my $response = $ua->get($baseuri);
		use Data::Dumper;
		croak "HTTP response not successful\n"
			unless $response->is_success;
		croak "Non-XHTML HTTP response\n"
			unless $response->content_type =~ m`^(text/(x|ht)ml)|(application/(xhtml\+xml|xml))$`;
		$xhtml = $response->decoded_content;
	}
	
	if (UNIVERSAL::isa($xhtml, 'XML::LibXML::Document'))
	{
		$DOMTree = $xhtml;
		$xhtml = $DOMTree->toString;
	}
	else
	{
		my $parser  = XML::LibXML->new;
		$parser->validation(0);
		$parser->recover(1);
		
		my $catalogue = dist_file('RDF-RDFa-Parser', 'catalogue/index.xml');
		$parser->load_catalog($catalogue)
			if -r $catalogue;
		
		$DOMTree = $parser->parse_string($xhtml);
	}
	
	$store = RDF::Trine::Store::DBI->temporary_store
		unless defined $store;

	my $this = {
			'xhtml'   => $xhtml,
			'baseuri' => $baseuri,
			'origbase' => $baseuri,
			'DOM'     => $DOMTree,
			'RESULTS' => RDF::Trine::Model->new($store),
			'bnodes'  => 0,
			'sub'     => {},
			'options' => {
				'alt_stylesheet'        => 0,
				'atom_elements'         => 0,
				'atom_parser'           => 0,
				'auto_config'           => 0,
				'embedded_rdfxml'       => 0,
				'full_uris'             => 0,
				'keywords'              => keywords('rdfa'),
				'graph'                 => 0,
				'graph_attr'            => 'graph',
				'graph_type'            => 'id',
				'graph_default'         => '_:RDFaDefaultGraph',
				'graph_default_trine'   => undef,  # not officially exposed
				'prefix_attr'           => 0,
				'prefix_bare'           => 0,
				'prefix_default'        => 0,
				'prefix_empty'          => 'http://www.w3.org/1999/xhtml/vocab#',
				'prefix_nocase'         => 0,
				'safe_anywhere'         => 0,
				'tdb_service'           => 0,
				'use_rtnlx'             => 0,
				'xhtml_base'            => 1,
				'xhtml_elements'        => 1,
				'xhtml_lang'            => 0,
				'xml_base'              => 0,
				'xml_lang'              => 1,
			},
			'Graphs'  => {},
		};
	bless $this, $class;
	
	foreach my $o (keys %$options)
	{
		$this->{'options'}->{$o} = $options->{$o};
	}
	
	$this->auto_config;

	# HTML <base> element.
	if ($this->{'options'}->{'xhtml_base'})
	{
		my @bases = $this->{DOM}->getElementsByTagName('base');
		my $base;
		foreach my $b (@bases)
		{
			if ($b->hasAttribute('href'))
			{
				$base = $b->getAttribute('href');
				$base =~ s/#.*$//g;
			}
		}
		$this->{'baseuri'} = $this->uri($base)
			if defined $base && length $base;
	}
	
	return $this;
}

=head2 Public Methods

=over 4

=item C<< $p->consume >>

The document is parsed for RDFa. Triples extracted from the document are passed
to the callbacks as each one is found; triples are made available in the model returned
by the C<graph> method.

This function returns the parser object itself, making it easy to abbreviate several of
RDF::RDFa::Parser's functions:

  my $iterator = RDF::RDFa::Parser->new($xhtml,$uri)
                 ->consume->graph->as_stream;

=cut

sub consume
{
	my $self = shift;
	
	if ($self->{'options'}->{'graph'})
	{
		$self->{'options'}->{'graph_attr'} = 'graph'
			unless defined $self->{'options'}->{'graph_attr'};
		$self->{'options'}->{'graph_type'} = 'id'
			unless defined $self->{'options'}->{'graph_type'};
		$self->{'options'}->{'graph_default'} = '_:RDFaDefaultGraph'
			unless defined $self->{'options'}->{'graph_default'};
		
		if ((substr $self->{'options'}->{'graph_default'}, 0, 2) eq '_:')
		{
			$self->{'options'}->{'graph_default_trine'} =
				RDF::Trine::Node::Blank->new( substr $self->{'options'}->{'graph_default'}, 2 );
		}
		else
		{
			$self->{'options'}->{'graph_default_trine'} =
				RDF::Trine::Node::Resource->new( $self->{'options'}->{'graph_default'} );
		}
	}
	
	$self->_consume;
	
	if ($self->{'options'}->{'atom_parser'} && $HAS_AWOL)
	{
		my $awol = XML::Atom::OWL->new( $self->dom , $self->uri , undef, $self->{'RESULTS'} );
		$awol->{'bnode_generator'} = $self;
		$awol->set_callbacks( $self->{'sub'} );
		$awol->consume;
	}
	
	return $self;
}

sub _consume
# http://www.w3.org/TR/rdfa-syntax/#sec_5.5.
{
	no warnings;

	my $this = shift;
	my $dom  = shift || $this->{DOM}->documentElement;
	
	return 0 unless ($dom->nodeType == XML_ELEMENT_NODE);
	
	# At the beginning of processing, an initial [evaluation context] is created
	my $base               = shift || $this->uri;
	my $parent_subject     = shift || $base;
	my $parent_object      = shift || undef;
	my $uri_mappings       = shift || {};
	my $incomplete_triples = shift || ();
	my $language           = shift || undef;
	my $graph              = shift || ($this->{'options'}->{'graph'} ? $this->{'options'}->{'graph_default'} : undef);
	my $xml_base           = shift || undef;
	
	# Processing begins by applying the processing rules below to the document
	# object, in the context of this initial [evaluation context]. All elements
	# in the tree are also processed according to the rules described below,
	# depth-first, although the [evaluation context] used for each set of rules
	# will be based on previous rules that may have been applied.

	my $current_element = $dom;

	# First, the local values are initialized
	my $recurse            = 1;
	my $skip_element       = 0;
	my $new_subject        = undef;
	my $current_object_resource = undef;
	my $local_uri_mappings = $uri_mappings;
	my $local_incomplete_triples = ();
	my $current_language   = $language;
	
	my $activity = 0;

	# MOVED THIS SLIGHTLY EARLIER IN THE PROCESSING so that it can apply
	# to RDF/XML chunks.
	#
	# The [current element] is also parsed for any language information, and
	# if present, [current language] is set accordingly.
	# Language information can be provided using the general-purpose XML
	# attribute @xml:lang .
	if ($this->{'options'}->{'xhtml_lang'}
	&& $current_element->hasAttribute('lang'))
	{
		$current_language = $current_element->getAttribute('lang')
			if valid_lang( $current_element->getAttribute('lang')) ;
	}
	if ($this->{'options'}->{'xml_lang'}
	&& $current_element->hasAttributeNS(XML_XML_NS, 'lang'))
	{
		$current_language = $current_element->getAttributeNS(XML_XML_NS, 'lang')
			if valid_lang($current_element->getAttributeNS(XML_XML_NS, 'lang'));
	}

	# EXTENSION
	# xml:base - important for RDF/XML extension
	if ($current_element->hasAttributeNS(XML_XML_NS, 'base'))
	{
		$xml_base = $current_element->getAttributeNS(XML_XML_NS, 'base');
		$xml_base =~ s/#.*$//g;
		$xml_base = $this->uri($xml_base);
	}
	my $hrefsrc_base = $base;
	if ($this->{'options'}->{'xml_base'}==2 && defined $xml_base)
	{
		$hrefsrc_base = $xml_base;
	}

	# EXTENSION
	# Parses embedded RDF/XML - mostly useful for non-XHTML documents, e.g. SVG.
	if ($this->{'options'}->{'embedded_rdfxml'}
	&& $current_element->localname eq 'RDF'
	&& $current_element->namespaceURI eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#')
	{
		return 1 if $this->{'options'}->{'embedded_rdfxml'}==2;

		my $g = $this->bnode;
		
		my $fake_lang = 0;
		unless ($current_element->hasAttributeNS(XML_XML_NS, 'lang'))
		{
			$current_element->setAttributeNS(XML_XML_NS, 'lang', $current_language);
			$fake_lang = 1;
		}
		
		my $rdfxml_base = $this->{'origbase'};
		$rdfxml_base = $base
			if $this->{'options'}->{'xhtml_base'}==2;
		$rdfxml_base = $xml_base
			if defined $xml_base;
		
		my $parser  = RDF::Trine::Parser->new('rdfxml');
		my $r       = $parser->parse(
			$rdfxml_base,
			$current_element->toStringEC14N,
			sub {
				my $st = shift;
				my ($s, $p, @o);
				
				$s = $st->subject->is_blank ?
					($g.'_'.$st->subject->blank_identifier) :
					$st->subject->uri_value ;
				$p = $st->predicate->is_blank ?
					($g.'_'.$st->predicate->blank_identifier) :
					$st->predicate->uri_value ;
				if ($st->object->is_literal)
				{
					$o[0] = $st->object->literal_value;
					$o[1] = $st->object->literal_datatype;
					$o[2] = $st->object->literal_value_language;
					
					$this->rdf_triple_literal(undef, $s, $p, @o,
						($this->{'options'}->{'graph'} ? $g : undef));
				}
				else
				{
					$o[0] = $st->object->is_blank ?
						($g.'_'.$st->object->blank_identifier) :
						$st->object->uri_value ;
					$this->rdf_triple(undef, $s, $p, @o,
						($this->{'options'}->{'graph'} ? $g : undef));
				}				
			});
			
		$current_element->removeAttributeNS(XML_XML_NS, 'lang')
			if ($fake_lang);
			
		return 1;
	}
	
	# Next the [current element] is parsed for [URI mapping]s and these are
	# added to the [local list of URI mappings]. Note that a [URI mapping] 
	# will simply overwrite any current mapping in the list that has the same
	# name
	#
	# Mappings are provided by @xmlns. The value to be mapped is set by
	# the XML namespace prefix, and the value to map is the value of the
	# attribute - a URI. Note that the URI is not processed in any way;
	# in particular if it is a relative path it is not resolved against
	# the current [base]. Authors are advised to follow best practice
	# for using namespaces, which includes not using relative paths.
	foreach my $A ($current_element->getAttributes)
	{
		my $attr = $A->getName;
		
		if ($attr =~ /^xmlns\:(.+)$/i)
		{
			my $pfx = $this->{'options'}->{'prefix_nocase'} ? (lc $1) : $1;
			my $uri = $A->getValue;
			if ($pfx =~ /^(xml|xmlns|_)$/i)
			{
				carp("Attempt to redefine CURIE prefix '$pfx' not allowed.");
			}
			elsif ($uri eq XML_XML_NS || $uri eq XML_XMLNS_NS)
			{
				carp("Attempt to define CURIE prefix for '$uri' not allowed.");
			}
			else
			{
				$this->{'sub'}->{'onprefix'}($this, $current_element, $pfx, $uri)
					if defined $this->{'sub'}->{'onprefix'};
	
				$local_uri_mappings->{$pfx} = $uri;
			}
		}
	}
	
	# EXTENSION
	# The rdfa.info wiki includes an additional proposed syntax for defining
	# prefixes. This uses a single @prefix attribute, making it more DTD
	# friendly. Let's support that too.
	if ($this->{'options'}->{'prefix_attr'}
	&& $current_element->hasAttribute('prefix'))
	{
		my $pfx_attr = $current_element->getAttribute('prefix');
		while ($pfx_attr =~ /^\s*(\S*)[\s\r\n]*=[\s\r\n]*(\S*)[\s\r\n]+/gs)
		{
			my $pfx = $this->{'options'}->{'prefix_nocase'} ? (lc $1) : $1;
			my $uri = $2;
			$this->{'sub'}->{'onprefix'}($this, $current_element, $pfx, $uri)
				if defined $this->{'sub'}->{'onprefix'};
			$local_uri_mappings->{$pfx} = $uri;
		}
	}
	
	# EXTENSION
	# KjetilK's named graphs.
	if ($this->{'options'}->{'graph'})
	{
		my ($xmlns, $attr) = ($this->{'options'}->{'graph_attr'} =~ /^(?:\{(.+)\})?(.+)$/);
		unless ($attr)
		{
			$xmlns = undef;
			$attr  = 'graph';
		}
		
		if ($this->{'options'}->{'graph_type'} eq 'id'
		&&  $current_element->hasAttributeNS($xmlns, $attr))
		{
			$graph = $this->uri('#' . $current_element->getAttributeNS($xmlns, $attr));
		}
		elsif ($this->{'options'}->{'graph_type'} eq 'about'
		&&  $current_element->hasAttributeNS($xmlns, $attr))
		{
			$graph = $this->uriOrSafeCurie($current_element->getAttributeNS($xmlns, $attr),
				$current_element, 'graph', $local_uri_mappings, $xml_base);
			
			$graph = $this->{'options'}->{'graph_default'}
				unless defined $graph;
		}
	}
	
	# If the [current element] contains no valid @rel or @rev URI, obtained
	# according to the section on CURIE and URI Processing, then the next step 
	# is to establish a value for [new subject]. Any of the attributes that 
	# can carry a resource can set [new subject]
	my $rel = $current_element->getAttribute('rel');
	$rel =~ s/(^\s+|\s+$)//g;
	$rel =~ s/\s+/ /g;
	my @rel = split / /, $rel;
	if ($this->{'options'}->{'alt_stylesheet'}
	&&  (grep /^alternate$/i, @rel)
	&&  (grep /^stylesheet$/i, @rel))   # EXTENSION: rel="alternate stylesheet"
	{
		@rel = grep !/^(alternate|stylesheet)$/i, @rel;
		push @rel, 'ALTERNATE-STYLESHEET';
	}
	my @REL;
	foreach my $r (@rel)
	{
		my $R = $this->curie($r, $current_element, 'rel', $local_uri_mappings, $xml_base);
		push @REL, $R if defined $R;
	}
	my $rev = $current_element->getAttribute('rev');
	$rev =~ s/(^\s+|\s+$)//g;
	$rev =~ s/\s+/ /g;
	my @rev = split / /, $rev;
	my @REV;
	foreach my $r (@rev)
	{
		my $R = $this->curie($r, $current_element, 'rev', $local_uri_mappings, $xml_base);
		push @REV, $R if defined $R;
	}
	
	unless ($current_element->hasAttribute('rel')
	||      $current_element->hasAttribute('rev'))
	{
		# [new subject] is set to the URI obtained from the first match
		# from the following rules:
		
		# by using the URI from @about, if present, obtained according to
		# the section on CURIE and URI Processing ; 
		if ($current_element->hasAttribute('about'))
			{ $new_subject = $this->uriOrSafeCurie($current_element->getAttribute('about'), $current_element, 'about', $local_uri_mappings, $xml_base); }
			
		# otherwise, by using the URI from @src, if present, obtained
		# according to the section on CURIE and URI Processing.
		if ($current_element->hasAttribute('src') && !defined $new_subject)
			{ $new_subject = $this->uri($current_element->getAttribute('src'), {'element'=>$current_element,'xml_base'=>$hrefsrc_base}); }
			
		# otherwise , by using the URI from @resource, if present, obtained
		# according to the section on CURIE and URI Processing ; 
		if ($current_element->hasAttribute('resource') && !defined $new_subject)
			{ $new_subject = $this->uriOrSafeCurie($current_element->getAttribute('resource'), $current_element, 'resource', $local_uri_mappings, $xml_base); }
			
		# otherwise , by using the URI from @href, if present, obtained
		# according to the section on CURIE and URI Processing.
		if ($current_element->hasAttribute('href') && !defined $new_subject)
			{ $new_subject = $this->uri($current_element->getAttribute('href'), {'element'=>$current_element,'xml_base'=>$hrefsrc_base}); }
			
		# If no URI is provided by a resource attribute, then the first
		# match from the following rules will apply: 
		unless (defined $new_subject)
		{
			# if the element is the head or body element then act as if
			# there is an empty @about present, and process it according to
			# the rule for @about, above; 
			if ($this->{'options'}->{'xhtml_elements'}
			&& ($current_element->namespaceURI eq 'http://www.w3.org/1999/xhtml')
			&& ($current_element->tagName eq 'head' || $current_element->tagName eq 'body'))
			{
				$new_subject = $this->uri;
			}

			# EXTENSION: atom elements
			elsif ($this->{'options'}->{'atom_elements'}
			&& ($current_element->namespaceURI eq 'http://www.w3.org/2005/Atom')
			&& ($current_element->tagName eq 'feed' || $current_element->tagName eq 'entry'))
			{
				$new_subject = $this->_atom_magic($current_element);
			}

			# if @instanceof is present, obtained according to the section
			# on CURIE and URI Processing , then [new subject] is set to be
			# a newly created [bnode]; 
			elsif ($current_element->hasAttribute('typeof')
				||  $current_element->hasAttribute('instanceof'))
			{
				$new_subject = $this->bnode($current_element);
			}
			
			# otherwise, if [parent object] is present, [new subject] is set
			# to the value of [parent object]. Additionally if @property is not
			# present then the [skip element] flag is set to 'true'; 
			elsif ($parent_object)
			{
				$new_subject = $parent_object;
				$skip_element = 1
					unless $current_element->hasAttribute('property');
			}
		}
	}
	
	# If the [current element] does contain a valid @rel or @rev URI, obtained 
	# according to the section on CURIE and URI Processing, then the next step 
	# is to establish both a value for [new subject] and a value for [current
	# object resource]:
	else
	{
		# [new subject] is set to the URI obtained from the first match
		# from the following rules: 
		
		# by using the URI from @about, if present, obtained according
		# to the section on CURIE and URI Processing; 
		if ($current_element->hasAttribute('about'))
			{ $new_subject = $this->uriOrSafeCurie($current_element->getAttribute('about'), $current_element, 'about', $local_uri_mappings, $xml_base); }
			
		# otherwise, by using the URI from @src, if present, obtained
		# according to the section on CURIE and URI Processing.
		if ($current_element->hasAttribute('src') && !defined $new_subject)
			{ $new_subject = $this->uri($current_element->getAttribute('src'), {'element'=>$current_element,'xml_base'=>$hrefsrc_base}); }

		# If no URI is provided then the first match from the following rules
		# will apply: 
		unless (defined $new_subject)
		{
			# if the element is the head or body element then act as if
			# there is an empty @about present, and process it according
			# to the rule for @about, above; 
			if ($this->{'options'}->{'xhtml_elements'}
			&& ($current_element->namespaceURI eq 'http://www.w3.org/1999/xhtml')
			&& ($current_element->tagName eq 'head' || $current_element->tagName eq 'body'))
			{
				$new_subject = $this->uri;
			}
			
			# EXTENSION: atom elements
			elsif ($this->{'options'}->{'atom_elements'}
			&& ($current_element->namespaceURI eq 'http://www.w3.org/2005/Atom')
			&& ($current_element->tagName eq 'feed' || $current_element->tagName eq 'entry'))
			{
				$new_subject = $this->_atom_magic($current_element);
			}
			
			# if @instanceof is present, obtained according to the section
			# on CURIE and URI Processing, then [new subject] is set to be a
			# newly created [bnode]; 
			elsif ($current_element->hasAttribute('typeof')
				||  $current_element->hasAttribute('instanceof'))
			{
				$new_subject = $this->bnode($current_element);
			}
			
			# otherwise, if [parent object] is present, [new subject] is set 
			# to that. 
			elsif ($parent_object)
			{
				$new_subject = $parent_object;
			}
		}
		
		# Then the [current object resource] is set to the URI obtained
		# from the first match from the following rules: 
		
		# by using the URI from @resource, if present, obtained according to
		# the section on CURIE and URI Processing; 
		if ($current_element->hasAttribute('resource'))
			{ $current_object_resource = $this->uriOrSafeCurie($current_element->getAttribute('resource'), $current_element, 'resource', $local_uri_mappings, $xml_base); }
			
		# otherwise, by using the URI from @href, if present, obtained according
		# to the section on CURIE and URI Processing.
		if ($current_element->hasAttribute('href') && !defined $current_object_resource)
			{ $current_object_resource = $this->uri($current_element->getAttribute('href'), {'element'=>$current_element,'xml_base'=>$hrefsrc_base}); }
		
		# Note that final value of the [current object resource] will either
		# be null (from initialization), a full URI or a bnode. 		
	}
	
	# If in any of the previous steps a [new subject] was set to a non-null
	# value, it is now used to provide a subject for type values
	if ($new_subject
	&& (  $current_element->hasAttribute('instanceof')
		|| $current_element->hasAttribute('typeof')))
	{
		# One or more 'types' for the [ new subject ] can be set by using
		# @instanceof. If present, the attribute must contain one or more
		# URIs, obtained according to the section on URI and CURIE Processing...
	
		my $instanceof = $current_element->getAttribute('typeof');
		$instanceof = $current_element->getAttribute('instanceof')
			unless defined $instanceof;
		$instanceof =~ s/\s+/ /g;
		$instanceof =~ s/(^\s|\s$)//g;
		my @instanceof = split / /, $instanceof;
		
		foreach my $instanceof (@instanceof)
		{
			my $rdftype = $this->curie($instanceof, $current_element, 'typeof', $local_uri_mappings, $xml_base);
			next unless defined $rdftype;
		
			# ... each of which is used to generate a triple as follows:
			#
			# subject
			#     [new subject] 
			# predicate
	    	#     http://www.w3.org/1999/02/22-rdf-syntax-ns#type 
			# object
			#     full URI of 'type' 

			$this->rdf_triple($current_element, $new_subject, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', $rdftype, $graph);
			$activity++;
		}

		# Note that none of this block is executed if there is no [new subject]
		# value, i.e., [new subject] remains null. 
	}
	
	# If in any of the previous steps a [current object resource] was set to
	# a non-null value, it is now used to generate triples
	if ($current_object_resource)
	{
		# Predicates for the [ current object resource ] can be set b
		# using one or both of the @rel and @rev attributes:
		#
		#    * If present, @rel will contain one or more URIs, obtained
		#      according to the section on CURIE and URI Processing each
		#      of which is used to generate a triple as follows:
		#
		#      subject
		#          [new subject] 
		#      predicate
		#          full URI 
		#      object
		#          [current object resource] 
		
		foreach my $r (@REL)
		{
			$this->rdf_triple($current_element, $new_subject, $r, $current_object_resource, $graph);
			$activity++;
		}

		#    * If present, @rev will contain one or more URIs, obtained
		#      according to the section on CURIE and URI Processing each
		#      of which is used to generate a triple as follows:
		#
		#      subject
		#          [current object resource] 
		#      predicate
		#          full URI 
		#      object
		#          [new subject] 
		
		foreach my $r (@REV)
		{
			$this->rdf_triple($current_element, $current_object_resource, $r, $new_subject, $graph);
			$activity++;
		}
	}
	
	# If however [current object resource] was set to null, but there are 
	# predicates present, then they must be stored as [incomplete triple]s, 
	# pending the discovery of a subject that can be used as the object. Also, 
	# [current object resource] should be set to a newly created [bnode]
	elsif ((scalar @REL) || (scalar @REV))
	{
		# Predicates for [incomplete triple]s can be set by using one or
		# both of the @rel and @rev attributes:
		#
		#    * If present, @rel must contain one or more URIs, obtained 
		#      according to the section on CURIE and URI Processing each
		#      of which is added to the [local list of incomplete triples]
		#      as follows:
		#
		#      predicate
		#          full URI 
		#      direction
		#          forward 
		
		foreach my $r (@REL)
		{
			push @$local_incomplete_triples, {
				predicate => $r,
				direction => 'forward',
				graph     => $graph
			};
		}
		
		#    * If present, @rev must contain one or more URIs, obtained
		#      according to the section on CURIE and URI Processing, each
		#      of which is added to the [local list of incomplete triples]
		#      as follows:
		#
		#      predicate
		#          full URI 
		#      direction
		#          reverse 
		
		foreach my $r (@REV)
		{
			push @$local_incomplete_triples, {
				predicate => $r,
				direction => 'reverse',
				graph     => $graph
			};
		}
		
		$current_object_resource = $this->bnode;
	}
	
	# The next step of the iteration is to establish any [current object
	# literal
	my @current_object_literal;
	
	my $prop = $current_element->getAttribute('property');
	$prop =~ s/(^\s+|\s+$)//g;
	$prop =~ s/\s+/ /g;
	my @prop = split / /, $prop;

	my $datatype = -1;
	if ($current_element->hasAttribute('datatype'))
		{ $datatype = $this->curie($current_element->getAttribute('datatype'), $current_element, 'datatype', $local_uri_mappings, $xml_base); }
		
	if (length $prop)
	{
		# Predicates for the [current object literal] can be set by using
		# @property. If present, one or more URIs are obtained according
		# to the section on CURIE and URI Processing and then the actual
		# literal value is obtained as follows:
		
		# as a [ plain literal ] if: 
		#
		# @content is present; 
		if ($current_element->hasAttribute('content'))
		{
			@current_object_literal = ($current_element->getAttribute('content'),
				($datatype == -1 ? undef : $datatype),
				$current_language);
		}
		
		# or all children of the [current element] are text nodes;
		# or there are no child nodes;
		# or the body of the [ current element ] does have non-text
		#    child nodes but @datatype is present, with an empty value. 
		elsif ((!$current_element->getElementsByTagName('*')) 
		||     ($datatype eq ''))
		{
			@current_object_literal = ($this->stringify($current_element),
				($datatype == -1 ? undef : $datatype),
				$current_language);
		}

		# Additionally, if there is a value for [current language] then
		# the value of the [plain literal] should include this language
		# information, as described in [RDF-CONCEPTS]. The actual literal
		# is either the value of @content (if present) or a string created
		# by concatenating the text content of each of the descendant
		# elements of the [current element] in document order. 
		
		# as an [XML literal] if:
		# 
      #    * the [current element] has any child nodes that are not simply
		#      text nodes, and @datatype is not present, or is present, but
		#      is set to rdf:XMLLiteral.
		#
	   # The value of the [XML literal] is a string created by serializing
		# to text, all nodes that are descendants of the [current element],
		# i.e., not including the element itself, and giving it a datatype of
		# rdf:XMLLiteral.
		elsif ($datatype eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral'
		|| ($datatype==-1 && $current_element->getElementsByTagName('*')))
		{
			@current_object_literal = ($this->xmlify($current_element, $current_language),
				'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral',
				$current_language);
			$recurse = 0;
		}
		
		# as a [typed literal] if:
		#
		#     * @datatype is present, and does not have an empty value.
		#
		# The actual literal is either the value of @content (if present)
		# or a string created by concatenating the value of all descendant
		# text nodes, of the [current element] in turn. The final string
		# includes the datatype URI, as described in [RDF-CONCEPTS], which
		# will have been obtained according to the section on CURIE and URI
		# Processing.
		elsif ($datatype != -1)
		{
			if ($current_element->hasAttribute('content'))
			{
				@current_object_literal = ($current_element->getAttribute('content'),
					($datatype == -1 ? undef : $datatype),
					$current_language);
			}
			else
			{
				@current_object_literal = ($this->stringify($current_element),
					($datatype == -1 ? undef : $datatype),
					$current_language);
			}
		}
		
		else
		{
			die("How did we get here??\n");
		}
	}
	
	foreach my $property (@prop)
	{
		# The [current object literal] is then used with each predicate to
		# generate a triple as follows:
		# 
		# subject
		#     [new subject] 
		# predicate
		#     full URI 
		# object
		#     [current object literal] 

		my $p = $this->curie($property, $current_element, 'property', $local_uri_mappings, $xml_base);
		next unless defined $p;
		
		$this->rdf_triple_literal($current_element, $new_subject, $p, @current_object_literal, $graph);

		$activity++;
		
		# Once the triple has been created, if the [datatype] of the
		# [current object literal] is rdf:XMLLiteral, then the [recurse]
		# flag is set to false.
		$recurse = 0
			if ($datatype eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral');
	}

	# If the [recurse] flag is 'true', all elements that are children of the 
	# [current element] are processed using the rules described here, using a 
	# new [evaluation context], initialized as follows
	my $flag = 0;
	if ($recurse)
	{
		foreach my $kid ($current_element->childNodes)
		{
			# If the [skip element] flag is 'true' then the new [evaluation context]
			# is a copy of the current context that was passed in to this level of
			# processing, with the [language] and [list of URI mappings] values
			# replaced with the local values; 
			if ($skip_element)
			{
				$flag = $this->_consume(
					$kid,
					$base,
					$parent_subject,
					$parent_object,
					$uri_mappings,
					$incomplete_triples,
					$language,
					$graph,
					$xml_base
				) || $flag;
			}
			
			# Otherwise, the values are: 
			else
			{
				$flag = $this->_consume(
					$kid,
					$base,
					$new_subject,
					(defined $current_object_resource ? $current_object_resource : (defined $new_subject ? $new_subject : $parent_subject)),
					$local_uri_mappings,
					$local_incomplete_triples,
					$current_language,
					$graph,
					$xml_base
				) || $flag;
			}
		}	
	}
	
#	# If the [skip element] flag is 'false', and either: the previous step
#	# resulted in a 'true' flag, or [new subject] was set to a non-null and
#	# non-bnode value, then any [incomplete triple]s within the current context
#	# should be completed:
#	if (!$skip_element && ($flag || ((defined $new_subject) && ($new_subject !~ /^bnodeXXX:/))))
#	{

	if (!$skip_element && defined $new_subject)
	{
		# Loop through list of incomplete triples...
		foreach my $it (@$incomplete_triples)
		{
			my $direction    = $it->{direction};
			my $predicate    = $it->{predicate};
			my $parent_graph = $it->{graph};
			
			if ($direction eq 'forward')
			{
				$this->rdf_triple($current_element, $parent_subject, $predicate, $new_subject, $parent_graph);
				$activity++;
			}
			else
			{
				$this->rdf_triple($current_element, $new_subject, $predicate, $parent_subject, $parent_graph);
				$activity++;
			}
		}
	}

	return 1 if ($activity || $new_subject || $flag);
	return 0;
}

=item C<< $p->set_callbacks(\%callbacks) >>

Set callback functions for the parser to call on certain events. These are only necessary if
you want to do something especially unusual.

  $p->set_callbacks({
    'pretriple_resource' => sub { ... } ,
    'pretriple_literal'  => sub { ... } ,
    'ontriple'           => undef ,
    'onprefix'           => \&some_function ,
    });

Either of the two pretriple callbacks can be set to the string 'print' instead of a coderef.
This enables built-in callbacks for printing Turtle to STDOUT.

For details of the callback functions, see the section CALLBACKS. C<set_callbacks> must
be used I<before> C<consume>. C<set_callbacks> itself returns a reference to the parser
object itself.

=cut

sub set_callbacks
# Set callback functions for handling RDF triples.
{
	my $this = shift;

	if ('HASH' eq ref $_[0])
	{
		$this->{'sub'} = $_[0];
		$this->{'sub'}->{'pretriple_resource'} = \&_print0
			if lc $this->{'sub'}->{'pretriple_resource'} eq 'print';
		$this->{'sub'}->{'pretriple_literal'} = \&_print1
			if lc $this->{'sub'}->{'pretriple_literal'} eq 'print';
	}
	else
	{
		carp "Unsupported set_callbacks call.\n";
	}
	
	return $this;
}

sub _print0
# Prints a Turtle triple.
{
	my $this    = shift;
	my $element = shift;
	my $subject = shift;
	my $pred    = shift;
	my $object  = shift;
	my $graph   = shift;
	
	if ($graph)
	{
		print "# GRAPH $graph\n";
	}
	if ($element)
	{
		printf("# Triple on element %s.\n", $element->nodePath);
	}
	else
	{
		printf("# Triple.\n");
	}

	printf("%s %s %s .\n",
		($subject =~ /^_:/ ? $subject : "<$subject>"),
		"<$pred>",
		($object =~ /^_:/ ? $object : "<$object>"));
	
	return undef;
}

sub _print1
# Prints a Turtle triple.
{
	my $this    = shift;
	my $element = shift;
	my $subject = shift;
	my $pred    = shift;
	my $object  = shift;
	my $dt      = shift;
	my $lang    = shift;
	my $graph   = shift;
	
	# Clumsy, but probably works.
	$object =~ s/\\/\\\\/g;
	$object =~ s/\n/\\n/g;
	$object =~ s/\r/\\r/g;
	$object =~ s/\t/\\t/g;
	$object =~ s/\"/\\\"/g;
	
	if ($graph)
	{
		print "# GRAPH $graph\n";
	}
	if ($element)
	{
		printf("# Triple on element %s.\n", $element->nodePath);
	}
	else
	{
		printf("# Triple.\n");
	}

	no warnings;
	printf("%s %s %s%s%s .\n",
		($subject =~ /^_:/ ? $subject : "<$subject>"),
		"<$pred>",
		"\"$object\"",
		(length $dt ? "^^<$dt>" : ''),
		((length $lang && !length $dt) ? "\@$lang" : '')
		);
	use warnings;
	
	return undef;
}

sub named_graphs
	{ croak "The named_graphs method is no longer supported (use constructor options instead)"; }

sub thing_described_by
	{ croak "The thing_described_by method is no longer supported (use the tdb_service option instead)"; }

sub rdf_triple
# Function only used internally.
{
	my $this = shift;

	my $suppress_triple = 0;
	$suppress_triple = $this->{'sub'}->{'pretriple_resource'}($this, @_)
		if defined $this->{'sub'}->{'pretriple_resource'};
	return if $suppress_triple;
	
	my $element   = shift;  # A reference to the XML::LibXML element being parsed
	my $subject   = shift;  # Subject URI or bnode
	my $predicate = shift;  # Predicate URI
	my $object    = shift;  # Resource URI or bnode
	my $graph     = shift;  # Graph URI or bnode (if named graphs feature is enabled)

	# First make sure the object node type is ok.
	my $to;
	if ($object =~ m/^_:(.*)/)
	{
		$to = RDF::Trine::Node::Blank->new($1);
	}
	else
	{
		$to = RDF::Trine::Node::Resource->new($object);
	}

	# Run the common function
	return $this->rdf_triple_common($element, $subject, $predicate, $to, $graph);
}

sub rdf_triple_literal
# Function only used internally.
{
	my $this = shift;

	my $suppress_triple = 0;
	$suppress_triple = $this->{'sub'}->{'pretriple_literal'}($this, @_)
		if defined $this->{'sub'}->{'pretriple_literal'};
	return if $suppress_triple;

	my $element   = shift;  # A reference to the XML::LibXML element being parsed
	my $subject   = shift;  # Subject URI or bnode
	my $predicate = shift;  # Predicate URI
	my $object    = shift;  # Resource Literal
	my $datatype  = shift;  # Datatype URI (possibly undef or '')
	my $language  = shift;  # Language (possibly undef or '')
	my $graph     = shift;  # Graph URI or bnode (if named graphs feature is enabled)

	# Now we know there's a literal
	my $to;
	
	# Work around bad Unicode handling in RDF::Trine.
	$object = encode_utf8($object);

	if (defined $datatype)
	{
		if ($datatype eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral')
		{
			if ($this->{'options'}->{'use_rtnlx'})
			{
				eval
				{
					require RDF::Trine::Node::Literal::XML;
					$to = RDF::Trine::Node::Literal::XML->new($element->childNodes);
				};
			}
			
			if ( $@ || !defined $to)
			{
				my $orig = $RDF::Trine::Node::Literal::USE_XMLLITERALS;
				$RDF::Trine::Node::Literal::USE_XMLLITERALS = 0;
				$to = RDF::Trine::Node::Literal->new($object, undef, $datatype);
				$RDF::Trine::Node::Literal::USE_XMLLITERALS = $orig;
			}
		}
		else
		{
			$to = RDF::Trine::Node::Literal->new($object, undef, $datatype);
		}
	}
	else
	{
		$to = RDF::Trine::Node::Literal->new($object, $language, undef);
	}

	# Run the common function
	$this->rdf_triple_common($element, $subject, $predicate, $to, $graph);
}

sub rdf_triple_common
# Function only used internally.
{
	my $this      = shift;  # A reference to the RDF::RDFa::Parser object
	my $element   = shift;  # A reference to the XML::LibXML element being parsed
	my $subject   = shift;  # Subject URI or bnode
	my $predicate = shift;  # Predicate URI
	my $to        = shift;  # RDF::Trine::Node Resource URI or bnode
	my $graph     = shift;  # Graph URI or bnode (if named graphs feature is enabled)

	# First, make sure subject and predicates are the right kind of nodes
	my $tp = RDF::Trine::Node::Resource->new($predicate);
	my $ts;
	if ($subject =~ m/^_:(.*)/)
	{
		$ts = RDF::Trine::Node::Blank->new($1);
	}
	else
	{
		$ts = RDF::Trine::Node::Resource->new($subject);
	}

	my $statement;

	# If we are configured for it, and graph name can be found, add it.
	if ($this->{'options'}->{'graph'} && $graph)
	{
		$this->{Graphs}->{$graph}++;
		
		my $tg;
		if ($graph =~ m/^_:(.*)/)
		{
			$tg = RDF::Trine::Node::Blank->new($1);
		}
		else
		{
			$tg = RDF::Trine::Node::Resource->new($graph);
		}

		$statement = RDF::Trine::Statement::Quad->new($ts, $tp, $to, $tg);
	}
	# If no graph name, just add triples
	else
	{
		$statement = RDF::Trine::Statement->new($ts, $tp, $to);
	}

	my $suppress_triple = 0;
	$suppress_triple = $this->{'sub'}->{'ontriple'}($this, $element, $statement)
		if ($this->{'sub'}->{'ontriple'});
	return if $suppress_triple;

	$this->{RESULTS}->add_statement($statement);
}

sub _atom_magic
{
	my $this    = shift;
	my $element = shift;
	
	return $this->bnode($element, 1);
}

sub stringify
# Function only used internally.
{
	my $this = shift;
	my $dom  = shift;
	
	if ($dom->nodeType == XML_TEXT_NODE)
	{
		return $dom->getData;
	}
	elsif ($dom->nodeType == XML_ELEMENT_NODE && lc($dom->tagName) eq 'img')
	{
		return $dom->getAttribute('alt');
	}
	elsif ($dom->nodeType == XML_ELEMENT_NODE)
	{
		my $rv = '';
		foreach my $kid ($dom->childNodes)
			{ $rv .= $this->stringify($kid); }
		return $rv;
	}

	return '';
}

sub xmlify
# Function only used internally.
{
	my $this = shift;
	my $dom  = shift;
	my $lang = shift;
	my $rv;
	
	foreach my $kid ($dom->childNodes)
	{
		my $fakelang = 0;
		if (($kid->nodeType == XML_ELEMENT_NODE) && defined $lang)
		{
			unless ($kid->hasAttributeNS(XML_XML_NS, 'lang'))
			{
				$kid->setAttributeNS(XML_XML_NS, 'lang', $lang);
				$fakelang++;
			}
		}
		
		$rv .= $kid->toStringEC14N(1);
		
		if ($fakelang)
		{
			$kid->removeAttributeNS(XML_XML_NS, 'lang');
		}
	}
	
	return $rv;
}

sub bnode
# Function only used internally.
{
	my $this    = shift;
	my $element = shift;
	my $save_me = shift || 0;
	
	if (defined $element
	and $this->{'saved_bnodes'}->{ $element->nodePath })
	{
		return $this->{'saved_bnodes'}->{ $element->nodePath };
	}
	
	return sprintf('http://thing-described-by.org/?%s#%s',
		$this->uri,
		$this->{element}->getAttribute('id'))
		if ($this->{options}->{tdb_service} && $element && length $element->getAttribute('id'));

	my $rv = sprintf('_:RDFaAutoNode%03d', $this->{bnodes}++);
	
	if ($save_me and defined $element)
	{
		$this->{'saved_bnodes'}->{ $element->nodePath } = $rv;
	}
	
	return $rv;
}

sub valid_lang
{
	my $value_to_test = shift;

	return 1 if (defined $value_to_test) && ($value_to_test eq '');
	return 0 unless defined $value_to_test;
	
	# Regex for recognizing RFC 4646 well-formed tags
	# http://www.rfc-editor.org/rfc/rfc4646.txt
	# http://tools.ietf.org/html/draft-ietf-ltru-4646bis-21

	# The structure requires no forward references, so it reverses the order.
	# It uses Java/Perl syntax instead of the old ABNF
	# The uppercase comments are fragments copied from RFC 4646

	# Note: the tool requires that any real "=" or "#" or ";" in the regex be escaped.

	my $alpha      = '[a-z]';      # ALPHA
	my $digit      = '[0-9]';      # DIGIT
	my $alphanum   = '[a-z0-9]';   # ALPHA / DIGIT
	my $x          = 'x';          # private use singleton
	my $singleton  = '[a-wyz]';    # other singleton
	my $s          = '[_-]';       # separator -- lenient parsers will use [_-] -- strict will use [-]

	# Now do the components. The structure is slightly different to allow for capturing the right components.
	# The notation (?:....) is a non-capturing version of (...): so the "?:" can be deleted if someone doesn't care about capturing.

	my $language   = '([a-z]{2,8}) | ([a-z]{2,3} $s [a-z]{3})';
	
	# ABNF (2*3ALPHA) / 4ALPHA / 5*8ALPHA  --- note: because of how | works in regex, don't use $alpha{2,3} | $alpha{4,8} 
	# We don't have to have the general case of extlang, because there can be only one extlang (except for zh-min-nan).

	# Note: extlang invalid in Unicode language tags

	my $script = '[a-z]{4}' ;   # 4ALPHA 

	my $region = '(?: [a-z]{2}|[0-9]{3})' ;    # 2ALPHA / 3DIGIT

	my $variant    = '(?: [a-z0-9]{5,8} | [0-9] [a-z0-9]{3} )' ;  # 5*8alphanum / (DIGIT 3alphanum)

	my $extension  = '(?: [a-wyz] (?: [_-] [a-z0-9]{2,8} )+ )' ; # singleton 1*("-" (2*8alphanum))

	my $privateUse = '(?: x (?: [_-] [a-z0-9]{1,8} )+ )' ; # "x" 1*("-" (1*8alphanum))

	# Define certain grandfathered codes, since otherwise the regex is pretty useless.
	# Since these are limited, this is safe even later changes to the registry --
	# the only oddity is that it might change the type of the tag, and thus
	# the results from the capturing groups.
	# http://www.iana.org/assignments/language-subtag-registry
	# Note that these have to be compared case insensitively, requiring (?i) below.

	my $grandfathered  = '(?:
			  (en [_-] GB [_-] oed)
			| (i [_-] (?: ami | bnn | default | enochian | hak | klingon | lux | mingo | navajo | pwn | tao | tay | tsu ))
			| (no [_-] (?: bok | nyn ))
			| (sgn [_-] (?: BE [_-] (?: fr | nl) | CH [_-] de ))
			| (zh [_-] min [_-] nan)
			)';

	# old:         | zh $s (?: cmn (?: $s Hans | $s Hant )? | gan | min (?: $s nan)? | wuu | yue );
	# For well-formedness, we don't need the ones that would otherwise pass.
	# For validity, they need to be checked.

	# $grandfatheredWellFormed = (?:
	#         art $s lojban
	#     | cel $s gaulish
	#     | zh $s (?: guoyu | hakka | xiang )
	# );

	# Unicode locales: but we are shifting to a compatible form
	# $keyvalue = (?: $alphanum+ \= $alphanum+);
	# $keywords = ($keyvalue (?: \; $keyvalue)*);

	# We separate items that we want to capture as a single group

	my $variantList   = $variant . '(?:' . $s . $variant . ')*' ;     # special for multiples
	my $extensionList = $extension . '(?:' . $s . $extension . ')*' ; # special for multiples

	my $langtag = "
			($language)
			($s ( $script ) )?
			($s ( $region ) )?
			($s ( $variantList ) )?
			($s ( $extensionList ) )?
			($s ( $privateUse ) )?
			";

	# Here is the final breakdown, with capturing groups for each of these components
	# The variants, extensions, grandfathered, and private-use may have interior '-'
	
	my $r = ($value_to_test =~ 
		/^(
			($langtag)
		 | ($privateUse)
		 | ($grandfathered)
		 )$/xi);
	return $r;
}

sub curie
# Function only used internally.
{
	my $page  = shift;
	my $str   = shift;
	my $dom   = shift;
	my $attr  = shift;
	my $maps  = shift;
	my $xbase = shift;
	my $xpath = $dom->getAttribute('_xpath') if ($dom);
	my $safe  = 0;
	
	# Keywords
	if (defined $page->{'options'}->{'keywords'}->{lc $attr}->{lc $str})
		{ return $page->{'options'}->{'keywords'}->{lc $attr}->{lc $str}; }
	
	# Blank nodes
	if ($str eq '_:' || $str eq '[_:]')
		{ return '_:RDFaX'; }
	elsif ($str =~ /^_:RDFa(.*)$/i || $str =~ /^\[_:RDFa(.*)\]$/i)
		{ return '_:RDFax'.$1; }
	elsif ($str =~ /^_:(.+)$/i || $str =~ /^\[_:(.+)\]$/i)
		{ return '_:'.$1; }

	# Cope with safe CURIEs.
	if ($page->{'options'}->{'safe_anywhere'} && $str =~ /^\[(.*)\]$/)
	{
		$str  = $1;
		$safe = 1;
	}

	# Default prefix
	if ($str =~ /^\:(\S*)$/)
		{ return $page->uri($page->{'options'}->{'prefix_empty'} . $1, {'element'=>$dom,'xml_base'=>$xbase}); }

	# If the string matches CURIE syntax, then resolve the CURIE.
	if ($str =~ /^([a-z_][^\s\:]*)?\:(\S*)$/i)
	{
		my $pfx = $page->{'options'}->{'prefix_nocase'} ? (lc ($1 || '')) : ($1 || '');
		my $sfx = $2;
		
		return $page->uri($maps->{$pfx}.$sfx, {'element'=>$dom,'xml_base'=>$xbase})
			if defined $maps->{$pfx};
			
		return undef if $safe;
	}
	
	if ($page->{'options'}->{'prefix_bare'})
	{
		my $pfx = $page->{'options'}->{'prefix_nocase'} ? (lc $str) : $str;
		
		return $page->uri($maps->{$pfx}, {'element'=>$dom,'xml_base'=>$xbase})
			if defined $maps->{$pfx};
	}
	
	if ($page->{'options'}->{'prefix_default'} and $str !~ /:/)
	{
		return $page->uri( $page->{'options'}->{'prefix_default'}.$str,
			{'element'=>$dom,'xml_base'=>$xbase} );
	}
	
	# If it wasn't a safe CURIE, then try falling back to an absolute URI.
	if (!$safe && $page->{'options'}->{'full_uris'})
	{
		return $page->uri($str, {'require-absolute'=>1,'element'=>$dom,'xml_base'=>$xbase});
	}
	
	return undef;
}

sub uriOrSafeCurie
# Function only used internally.
{
	my $page  = shift;
	my $str   = shift;
	my $dom   = shift;
	my $attr  = shift;
	my $maps  = shift;
	my $xbase = shift;
	my $xpath = $dom->getAttribute('_xpath') if ($dom);
	my $safe  = 0;

	# Keywords
	if (defined $page->{'options'}->{'keywords'}->{lc $attr}->{lc $str})
		{ return $page->{'options'}->{'keywords'}->{lc $attr}->{lc $str}; }
	
	# Blank nodes
	if ($str eq '_:' || $str eq '[_:]')
		{ return '_:RDFaX'; }
	elsif ($str =~ /^_:RDFa(.*)$/i || $str =~ /^\[_:RDFa(.*)\]$/i)
		{ return '_:RDFax'.$1; }
	elsif ($str =~ /^_:(.+)$/i || $str =~ /^\[_:(.+)\]$/i)
		{ return '_:'.$1; }

	# Cope with safe CURIEs.
	if ($str =~ /^\[(.*)\]$/)
	{
		$str  = $1;
		$safe = 1;
	}

	# Default prefix
	if ($str =~ /^\:(\S*)$/)
		{ return $page->uri($page->{'options'}->{'prefix_empty'} . $1, {'element'=>$dom,'xml_base'=>$xbase}); }

	# Only safe CURIEs allowed
	if ($safe)
	{
		# If the string matches CURIE syntax, then resolve the CURIE.
		if ($str =~ /^([a-z_][^\s\:]*)?\:(\S*)$/i)
		{
			my $pfx = $page->{'options'}->{'prefix_nocase'} ? (lc ($1 || '')) : ($1 || '');
			my $sfx = $2;
			
			return $page->uri($maps->{$pfx}.$sfx, {'element'=>$dom,'xml_base'=>$xbase})
				if defined $maps->{$pfx};
			
			return undef;
		}
		
		if ($page->{'options'}->{'prefix_bare'})
		{
			my $pfx = $page->{'options'}->{'prefix_nocase'} ? (lc $str) : $str;
			
			return $page->uri($maps->{$pfx}, {'element'=>$dom,'xml_base'=>$xbase})
				if defined $maps->{$pfx};
		}
		
		if ($page->{'options'}->{'prefix_default'} and $str !~ /:/)
		{
			return $page->uri( $page->{'options'}->{'prefix_default'}.$str,
				{'element'=>$dom,'xml_base'=>$xbase} );
		}
	}
	
	# If it wasn't a safe CURIE, then fall back to a URI.
	else
	{
		return $page->uri($str, {'element'=>$dom,'xml_base'=>$xbase});
	}
	
	return undef;
}


=item C<< $p->graph( [ $graph_name ] )  >>

Without a graph name, this method will return an RDF::Trine::Model
object with all statements of the full graph. As per the RDFa
specification, it will always return an graph containing all the statements
of the RDFa document. If the model contains multiple graphs, all
statements will be returned unless a graph name is specified.

It will also take an optional graph URI as argument, and return an
RDF::Trine::Model tied to a temporary storage with all statements in that
graph. This feature is only useful if you're using named graphs.

It makes sense to call C<consume> before calling C<graph>. Otherwise
you'll just get an empty graph.

=cut

sub graph
{
	my $self = shift;
	my $graph = shift;
	if (defined($graph))
	{
		my $tg;
		if ($graph =~ m/^_:(.*)/)
		{
			$tg = RDF::Trine::Node::Blank->new($1);
		}
		else
		{
			$tg = RDF::Trine::Node::Resource->new($graph, $self->{baseuri});
		}
		my $storage = RDF::Trine::Store::DBI->temporary_store;
		my $m = RDF::Trine::Model->new($storage);
		my $i = $self->{RESULTS}->get_statements(undef, undef, undef, $tg);
		while (my $statement = $i->next)
		{
			$m->add_statement($statement);
		}
		return $m;
	}
	else
	{
		return $self->{RESULTS};
	}
}

=item C<< $p->graphs >>

Will return a hashref of all named graphs, where the graph name is a
key and the value is a RDF::Trine::Model tied to a temporary storage.

This method is only useful if you're using named graphs.

It makes sense to call C<consume> before calling C<graphs>. Otherwise
you'll just get an empty hashref.

=cut

sub graphs
{
	my $self = shift;
	my @graphs = keys(%{$self->{Graphs}});
	my %result;
	foreach my $graph (@graphs)
	{
		$result{$graph} = $self->graph($graph);
	}
	return \%result;
}

=item C<< $p->dom >>

Returns the parsed XML::LibXML::Document.

=cut

sub dom
{
	my $this = shift;
	return $this->{DOM};
}

=item C<< $p->xhtml >>

Returns the XHTML/XML source of the document being parsed.

=cut

sub xhtml
{
	my $this = shift;
	return $this->{xhtml};
}

=item C<< $p->uri( [$other_uri] ) >>

Returns the base URI of the document being parsed. This will usually be the
same as the base URI provided to the constructor, but may differ if the
document contains a <base> HTML element.

Optionally it may be passed a parameter - an absolute or relative URI - in
which case it returns the same URI which it was passed as a parameter, but
as an absolute URI, resolved relative to the document's base URI.

This seems like two unrelated functions, but if you consider the consequence
of passing a relative URI consisting of a zero-length string, it in fact makes
sense.

=cut

sub uri
{
	my $this  = shift;
	my $param = shift || '';
	my $opts  = shift || {};
	
	if ((ref $opts) =~ /^XML::LibXML/)
	{
		my $x = {'element' => $opts};
		$opts = $x;
	}
	
	if ($param =~ /^([a-z][a-z0-9\+\.\-]*)\:/i)
	{
		# seems to be an absolute URI, so can safely return "as is".
		return $param;
	}
	elsif ($opts->{'require-absolute'})
	{
		return undef;
	}
	
	my $base = $this->{baseuri};
	if ($this->{'options'}->{'xml_base'})
	{
		$base = $opts->{'xml_base'} || $this->{baseuri};
	}
	
	my $url = url $param, $base;
	my $rv  = $url->abs->as_string;

	# This is needed to pass test case 0114.
	while ($rv =~ m!^(http://.*)(\.\./|\.)+(\.\.|\.)?$!i)
	{
		$rv = $1;
	}
	
	return $rv;
}

sub auto_config
# Internal use only.
{
	my $this  = shift;
	my $count = 0;
	
	return undef unless ($this->{'options'}->{'auto_config'});

	my $xpc = XML::LibXML::XPathContext->new;
	$xpc->registerNs('x', 'http://www.w3.org/1999/xhtml');
	my $nodes   = $xpc->find('//x:meta[@name="http://search.cpan.org/dist/RDF-RDFa-Parser/#auto_config"]/@content', $this->{'DOM'}->documentElement);
	my $optstr = '';
	foreach my $node ($nodes->get_nodelist)
	{
		$optstr .= '&' . $node->getValue;
	}
	$optstr =~ s/^\&//;
	my $options = parse_axwfue($optstr);
	
	foreach my $o (keys %$options)
	{
		next unless $o=~ /^(alt_stylesheet | embedded_rdfxml | full_uris |
			keywords | prefix_attr | prefix_bare | prefix_empty | prefix_nocase |
			safe_anywhere | tdb_service | xhtml_base | xhtml_elements | xhtml_lang |
			xml_base | xml_lang | graph | graph_attr | graph_type | graph_default |
			prefix_default )$/ix;
		
		$count++;
			
		if ($o =~ /^keywords$/i)
		{
			$this->{'options'}->{lc $o} = keywords($options->{$o});
		}
		else
		{
			$this->{'options'}->{lc $o} = $options->{$o};
		}
	}
	
	return $count;
}

sub parse_axwfue
# Internal use only
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

=back

=head2 Utility Method

=over 4

=item C<< $structure = RDF::RDFa::Parser::keywords(@bundles) >>

Without any parameters, gets an empty structure for keywords. Passing
additional strings adds certain bundles of predefined keywords to the
structure.

  my $keyword_structure = RDF::RDFa::Parser::keywords(
	'xhtml', 'xfn', 'grddl');

This is useful to create a keyword structure may be provided as an
option to the RDF::RDFa::Parser constructor. You probably want to
leave this alone unless you know what you're doing.

Bundles include: rdfa, html5, html4, html32, iana, grddl, xfn.

=back

=cut

sub keywords
{
	my $terms = join(' ', @_);
	
	my $KW = {
		'about'     => {},
		'resource'  => {},
		'rel'       => {},
		'rev'       => {},
		'property'  => {},
		'datatype'  => {},
		'typeof'    => {},
		'graph'     => {},
		};
	
	if ($terms =~ /\b(rdfa)\b/i)
	{
		foreach my $attr (qw(rel rev))
		{
			foreach my $word (qw(alternate appendix bookmark cite
				chapter contents copyright first glossary help
				icon index last license meta next p3pv1
				prev role section stylesheet subsection start top up))
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
			foreach my $word (qw(Alternate Stylesheet Start Next Prev
				Contents Index Glossary Copyright Chapter Section
				Subsection Appendix Help Bookmark))
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
				enclosure first glossary help index last license
				next next-archive payment prev previous prev-archive
				related replies section self service start stylesheet
				subsection up via))
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
	
	if ($terms =~ /\b(xfn)\b/i)
	{
		foreach my $attr (qw(rel rev))
		{
			foreach my $word (qw(contact acquaintance friend met
				co-worker colleague co-resident neighbor child
				parent sibling spouse kin muse crush date
				sweetheart me))
			{
				$KW->{ $attr }->{ lc $word } = 'http://vocab.sindice.com/xfn#' . $word . "-hyperlink"
					unless defined $KW->{ $attr }->{ lc $word };
			}
		}
	}
	
	return $KW;
}

=head2 Constants

These are not really constants, but functions which consistently return the same thing.
You can mostly think of them as constants though.

=over 4

=item RDF::RDFa::Parser::OPTS_XHTML

Suggested options hashref for parsing XHTML.

=cut

sub OPTS_XHTML
{
	return {};
}

=item RDF::RDFa::Parser::OPTS_HTML4

Suggested options hashref for parsing HTML 4.x.

=cut

sub OPTS_HTML4
{
	return {
		'prefix_nocase'=>1,
		'keywords'=>keywords('rdfa html4'),
		'xml_lang'=>0,
		'xhtml_lang'=>1,
		};
}

=item RDF::RDFa::Parser::OPTS_HTML5

Suggested options hashref for parsing HTML5.

=cut

sub OPTS_HTML5
{
	return {
		'prefix_nocase'=>1,
		'keywords'=>keywords('rdfa html5'),
		'alt_stylesheet'=>1,
		'xhtml_lang'=>1,
		};
}

=item RDF::RDFa::Parser::OPTS_SVG

Suggested options hashref for parsing SVG.

=cut

sub OPTS_SVG
{
	return &OPTS_XML;
}

=item RDF::RDFa::Parser::OPTS_ATOM

Suggested options hashref for parsing Atom / DataRSS.

=cut

sub OPTS_ATOM
{
	my $opts = &OPTS_XML;
	$opts->{atom_elements} = 1;
	$opts->{keywords} = keywords('rdfa iana');
	return $opts;
}

=item RDF::RDFa::Parser::OPTS_XML

Suggested options hashref for parsing generic XML.

=cut

sub OPTS_XML
{
	return {
		'xhtml_elements'  => 0,
		'xml_base'        => 2,
		'xhtml_base'      => 0,
		'embedded_rdfxml' => 1,
		};
}

1;

=back

=head1 CALLBACKS

Several callback functions are provided. These may be set using the C<set_callbacks> function,
which taskes a hashref of keys pointing to coderefs. The keys are named for the event to fire the
callback on.

=head2 pretriple_resource

This is called when a triple has been found, but before preparing the triple for
adding to the model. It is only called for triples with a non-literal object value.

The parameters passed to the callback function are:

=over 4

=item * A reference to the C<RDF::RDFa::Parser> object

=item * A reference to the C<XML::LibXML::Element> being parsed

=item * Subject URI or bnode (string)

=item * Predicate URI (string)

=item * Object URI or bnode (string)

=item * Graph URI or bnode (string or undef)

=back

The callback should return 1 to tell the parser to skip this triple (not add it to
the graph); return 0 otherwise.

=head2 pretriple_literal

This is the equivalent of pretriple_resource, but is only called for triples with a
literal object value.

The parameters passed to the callback function are:

=over 4

=item * A reference to the C<RDF::RDFa::Parser> object

=item * A reference to the C<XML::LibXML::Element> being parsed

=item * Subject URI or bnode (string)

=item * Predicate URI (string)

=item * Object literal (string)

=item * Datatype URI (string or undef)

=item * Language (string or undef)

=item * Graph URI or bnode (string or undef)

=back

Beware: sometimes both a datatype I<and> a language will be passed. 
This goes beyond the normal RDF data model.)

The callback should return 1 to tell the parser to skip this triple (not add it to
the graph); return 0 otherwise.

=head2 ontriple

This is called once a triple is ready to be added to the graph. (After the pretriple
callbacks.) The parameters passed to the callback function are:

=over 4

=item * A reference to the C<RDF::RDFa::Parser> object

=item * A reference to the C<XML::LibXML::Element> being parsed

=item * An RDF::Trine::Statement object.

=back

The callback should return 1 to tell the parser to skip this triple (not add it to
the graph); return 0 otherwise. The callback may modify the RDF::Trine::Statement
object.

=head2 onprefix

This is called when a new CURIE prefix is discovered. The parameters passed
to the callback function are:

=over 4

=item * A reference to the C<RDF::RDFa::Parser> object

=item * A reference to the C<XML::LibXML::Element> being parsed

=item * The prefix (string, e.g. "foaf")

=item * The expanded URI (string, e.g. "http://xmlns.com/foaf/0.1/")

=back

The return value of this callback is currently ignored, but you should return
0 in case future versions of this module assign significance to the return value.

=head1 FEATURES

=head2 HTML Support

The constructor function parses incoming markup as well-formed XML and will
croak if given tag-soup HTML (or even valid HTML in most cases). However,
you can pass the constructor an XML::LibXML::Document object instead of
markup: this object could have been constructed from an HTML document.

The L<HTML::HTML5::Parser> module is able to create an XML::LibXML::Document
from tag-soup HTML; and the L<HTML::HTML5::Sanity> module may be able
to assist you in fixing namespace and other oddities in the resulting document. Here's
an example of using both with RDF::RDFa::Parser:

  use HTML::HTML5::Parser;
  use HTML::HTML5::Sanity qw(fix_document);
  use RDF::RDFa::Parser;
  
  my $html_parser = HTML::HTML5::Parser->new;
  my $document    = $html_parser->parse_string($html);
  my $fixed_doc   = fix_document($document);
  
  my $rdfa_parser = RDF::RDFa::Parser->new(
                      $fixed_doc,
                      'http://example.com/doc.html',
                      RDF::RDFa::Parser::OPTS_HTML5);

Note that the C<RDF::RDFa::Parser::OPTS_HTML4> and
C<RDF::RDFa::Parser::OPTS_HTML5> constants provide suggested
settings for parsing HTML. In particular, they make CURIE
prefixes case-insensitive, add support for the HTML lang
attribute (instead of or as well as the xml:lang attribute),
and bring in support for additional rel/rev keywords.

=head2 Atom / DataRSS

When processing Atom, if the 'atom_elements' option is switched on, RDF::RDFa::Parser
will treat <feed> and <entry> elements specially. This is similar to the special support
for <head> and <body> mandated by the XHTML+RDFa Recommendation. Essentially <feed> and
<entry> elements are assumed to have an imaginary "about" attribute which has its value
set to a brand new blank node.

If the 'atom_parser' option is switched on, RDF::RDFa::Parser fully parses Atom feeds
and entries, using the XML::Atom::OWL package. The two modules attempt to work together
in assigning blank node identifiers consistently, etc. If XML::Atom::OWL is not installed,
then this option will be silently ignored.

The C<RDF::RDFa::Parser::OPTS_ATOM> constant provides suggested
settings for parsing Atom. It switches on the 'atom_elements' option
(but not 'atom_parser'), adds support for IANA-registered rel/rev
keywords, switches off support for some XHTML-specific features,
enables processing of the xml:base attribute, and adds support for
embedded chunks of RDF/XML.

Generally speaking, adding RDFa attributes to elements in the Atom namespace themselves
can result in some slightly muddy semantics. It's best to use an extension namespace and
add the RDFa attributes to elements in that namespace. DataRSS provides a good
example of this. See L<http://developer.yahoo.com/searchmonkey/smguide/datarss.html>.

=head2 SVG

The SVG Tiny 1.2 specification makes the use of RDFa attributes within
SVG images valid.

The C<RDF::RDFa::Parser::OPTS_SVG> constant provides suggested
settings for parsing SVG. It switches off support for some
XHTML-specific features, enables processing of the xml:base attribute,
and adds support for embedded chunks of RDF/XML.

=head2 Embedded RDF/XML

Though a rarely used feature, XHTML allows other XML markup languages
to be directly embedded into it. In particular, chunks of RDF/XML can
be included in XHTML. While this is not common in XHTML, it's seen quite
often in SVG and other XML markup languages.

When RDF::RDFa::Parser encounters a chunk of RDF/XML in a document
it's parsing (i.e. an element called 'RDF' with namespace
'http://www.w3.org/1999/02/22-rdf-syntax-ns#'), there are three different
courses of action it can take:

=over 4

=item 0. Continue straight through it.

This is the behaviour that XHTML+RDFa seems to suggest is the right
option. It should mostly not do any harm: triples encoded in RDF/XML
will be generally ignored (though the chunk itself could theoretically
end up as part of an XML literal). It will waste a bit of time though.

=item 1. Skip the chunk.

This will skip over the RDF element entirely, and thus save you a
bit of time.

=item 2. Parse the RDF/XML.

The parser will parse the RDF/XML properly. If named graphs are
enabled, any triples will be added to a separate graph. This is
the behaviour that SVG Tiny 1.2 seems to suggest is the correct
thing to do.

=back

You can decide which path to take by setting the 'embedded_rdfxml'
option in the constructor. For HTML and XHTML, you probably want
to set embedded_rdfxml to '0' (the default) or '1'. For other XML
markup languages (e.g. SVG or Atom), then you probably want to
set it to '2'.

If you use the C<RDF::RDFa::Parser::OPTS_XHTML>, 
C<RDF::RDFa::Parser::OPTS_SVG> etc constants, they should mostly
do the right thing.

=head2 Named Graphs

The parser has support for named graphs within a single RDFa
document. To switch this on, use the 'graph' option in the
constructor.

The name of the attribute which indicates graph URIs is by
default 'graph', but can be changed using the 'graph_attr'
option. This option accepts clark notation to specify a
namespaced attribute. By default, the attribute value is
interpreted as a fragment identifier (like the 'id' attribute),
but if you set 'graph_type' to 'about', it will be treated
as a URI or safe CURIE (like the 'about' attribute).

The 'graph_default' option allows you to set the default
graph URI/bnode identifier.

Once you're using named graphs, the C<graphs> method becomes
useful: it returns a hashref of { graph_uri => trine_model } pairs.
The optional parameter to the C<graph> method also becomes useful.

See also L<http://buzzword.org.uk/2009/rdfa4/spec>.

=head2 Auto Config

RDF::RDFa::Parser has a lot of different options that can be switched
on and off. Sometimes it might be useful to allow the page being
parsed to control some of the options. If you switch on the 'auto_config'
option, pages can do this.

A page can set options using a specially crafted E<lt>metaE<gt> tag:

  <meta name="http://search.cpan.org/dist/RDF-RDFa-Parser/#auto_config"
     content="xhtml_lang=1&amp;keywords=rdfa+html5+html4+html32" />

Note that the C<content> attribute is an application/x-www-form-urlencoded
string (which must then be HTML-escaped of course). Semicolons may be used
instead of ampersands, as these tend to look nicer:

  <meta name="http://search.cpan.org/dist/RDF-RDFa-Parser/#auto_config"
     content="xhtml_lang=1;keywords=rdfa+html5+html4+html32" />

Any option allowed in the constructor may be given using auto config,
except 'use_rtnlx', and of course 'auto_config' itself. 

It's possible to use auto config outside XHTML (e.g. in Atom or
SVG) using namespaces:

  <xhtml:meta xmlns:xhtml="http://www.w3.org/1999/xhtml"
     name="http://search.cpan.org/dist/RDF-RDFa-Parser/#auto_config"
     keywords="iana+rdfa;xml_base=2;atom_elements=1" />

=head1 BUGS

RDF::RDFa::Parser 0.21 passed all approved tests in the XHTML+RDFa
test suite at the time of its release.

RDF::RDFa::Parser 0.22 (used in conjunction with HTML::HTML5::Parser
0.01 and HTML::HTML5::Sanity 0.01) additionally passes all approved
tests in the HTML4+RDFa and HTML5+RDFa test suites at the time of
its release; except test cases 0113 and 0121, which the author of
this module believes mandate incorrect HTML parsing.

Please report any bugs to L<http://rt.cpan.org/>.

Common gotchas:

=over 8

=item * Are you using the XML catalogue?

RDF::RDFa::Parser maintains a locally cached version of the XHTML+RDFa
DTD. This will normally be within your Perl module directory, in a subdirectory
named "auto/share/dist/RDF-RDFa-Parser/catalogue/".
If this is missing, the parser should still work, but will be very slow.

=back

=head1 SEE ALSO

L<XML::LibXML>, L<RDF::Trine>, L<HTML::HTML5::Parser>, L<HTML::HTML5::Sanity>,
L<XML::Atom::OWL>.

L<http://www.perlrdf.org/>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 ACKNOWLEDGEMENTS

Kjetil Kjernsmo E<lt>kjetilk@cpan.orgE<gt> wrote much of the stuff for
building RDF::Trine models. Neubert Joachim taught me to use XML
catalogues, which massively speeds up parsing of XHTML files that have
DTDs.

=head1 COPYRIGHT

Copyright 2008-2010 Toby Inkster

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
