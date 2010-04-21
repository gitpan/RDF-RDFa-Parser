package RDF::RDFa::Parser;

=head1 NAME

RDF::RDFa::Parser - flexible RDFa parser

=head1 SYNOPSIS

 use RDF::RDFa::Parser;
 
 $parser = RDF::RDFa::Parser->new($xhtml, $uri)->consume;
 $graph  = $parser->graph;

=cut

use Encode qw(encode_utf8);
use File::ShareDir qw(dist_file);
use LWP::UserAgent;
use RDF::RDFa::Parser::Config;
use RDF::RDFa::Parser::Profile;
use RDF::Trine 0.118;
use Storable qw/dclone/;
use URI::Escape;
use URI::URL;
use XML::LibXML qw(:all);
use XML::RegExp;
use constant {
	ERR_WARNING  => 'w',
	ERR_ERROR    => 'e',
	};
use constant {
	ERR_CODE_PROFILE_UNUSABLE      =>  0x0101,
	ERR_CODE_PROFILE_DISABLED      =>  0x0102,
	ERR_CODE_RDFXML_MUDDLE         =>  0x0201,
	ERR_CODE_RDFXML_MESS           =>  0x0202,
	ERR_CODE_PREFIX_BUILTIN        =>  0x0301,
	ERR_CODE_PREFIX_ILLEGAL        =>  0x0302,
	ERR_CODE_PREFIX_DISABLED       =>  0x0303,
	ERR_CODE_INSTANCEOF_USED       =>  0x0401,
	ERR_CODE_INSTANCEOF_OVERRULED  =>  0x0402,
	ERR_CODE_CURIE_FELLTHROUGH     =>  0x0501,
	ERR_CODE_CURIE_UNDEFINED       =>  0x0502,
	ERR_CODE_BNODE_WRONGPLACE      =>  0x0601,
	ERR_CODE_VOCAB_DISABLED        =>  0x0701,
	ERR_CODE_LANG_INVALID          =>  0x0801,
	};
use strict;
use 5.008;

=head1 VERSION

1.09_02

=cut

our $VERSION = '1.09_02';
our $HAS_AWOL;

BEGIN
{
	eval "use XML::Atom::OWL;";
	$HAS_AWOL = $@ ? 0 : 1;
}

=head1 DESCRIPTION

=head2 Constructor

=over 4

=item C<< $p = RDF::RDFa::Parser->new($xhtml, $baseuri, $config, $storage) >>

This method creates a new RDF::RDFa::Parser object and returns it.

The $xhtml variable may contain an XHTML/XML string, or a
XML::LibXML::Document. If a string, the document is parsed using
XML::LibXML::Parser, which will throw an exception if it is not
well-formed. RDF::RDFa::Parser does not catch the exception.
If $xhtml is undef, then RDF::RDFa::Parser will fetch $baseuri to
obtain the document to be parsed.

The base URI is needed to resolve relative URIs found in the document.

$config optionally holds an RDF::RDFa::Parser::Config object which
determines the set of rules used to parse the RDFa. It defaults to
XHTML+RDFa 1.0.

$storage optionally holds an RDF::Trine::Store object. If undef, then
a new temporary store is created.

=back

=cut

sub new
{
	my $class   = shift;
	my $xhtml   = shift;
	my $baseuri = shift;
	my $config  = shift;
	my $store   = shift || undef;
	my $DOMTree;

	unless (defined $xhtml)
	{
		my $ua = LWP::UserAgent->new;
		$ua->agent(sprintf('%s/%s ', __PACKAGE__, $VERSION));
		$ua->default_header("Accept" => "application/xhtml+xml, application/xml;q=0.1, text/xml;q=0.1");
		my $response = $ua->get($baseuri);
		use Data::Dumper;
		die "HTTP response not successful\n"
			unless $response->is_success;
		die "Unknown HTTP response media type\n"
			unless $response->content_type =~ m`^(text/(x|ht)ml)|(application/(atom\+xml|xhtml\+xml|xml)|image/svg\+xml)$`;
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

	if (!defined $config)
	{
		$config = RDF::RDFa::Parser::Config->new;
	}
	elsif (!UNIVERSAL::isa($config, 'RDF::RDFa::Parser::Config'))
	{
		$config = RDF::RDFa::Parser::Config->new(undef, undef, %$config);
	}

	$store = RDF::Trine::Store::Memory->temporary_store
		unless defined $store;

	my $self = bless {
		'xhtml'    => $xhtml,
		'baseuri'  => $baseuri,
		'origbase' => $baseuri,
		'dom'      => $DOMTree,
		'model'    => RDF::Trine::Model->new($store),
		'bnodes'   => 0,
		'sub'      => {},
		'options'  => $config,
		'Graphs'   => {},
		'errors'   => [],
		}, $class;
	
	$config->auto_config($DOMTree);

	# HTML <base> element.
	if ($self->{'options'}->{'xhtml_base'})
	{
		my @bases = $self->dom->getElementsByTagName('base');
		my $base;
		foreach my $b (@bases)
		{
			if ($b->hasAttribute('href'))
			{
				$base = $b->getAttribute('href');
				$base =~ s/#.*$//g;
			}
		}
		$self->{'baseuri'} = $self->uri($base)
			if defined $base && length $base;
	}
	
	return $self;
}

=head2 Public Methods

=over 4

=item C<< $p->consume >>

The document is parsed for RDFa. Triples extracted from the document are passed
to the callbacks as each one is found; triples are made available in the model returned
by the C<graph> method.

This method returns the parser object itself, making it easy to abbreviate several of
RDF::RDFa::Parser's methods:

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
	
	$self->_consume_element($self->dom->documentElement, init=>1);
	
	if ($self->{'options'}->{'atom_parser'} && $HAS_AWOL)
	{
		my $awol = XML::Atom::OWL->new( $self->dom , $self->uri , undef, $self->{'model'} );
		$awol->{'bnode_generator'} = $self;
		$awol->set_callbacks( $self->{'sub'} );
		$awol->consume;
	}
	
	return $self;
}

sub _consume_element
# http://www.w3.org/TR/rdfa-syntax/#sec_5.5.
{
	no warnings;
	my $self = shift;

	# Processing begins by applying the processing rules below to the document
	# object, in the context of this initial [evaluation context]. All elements
	# in the tree are also processed according to the rules described below,
	# depth-first, although the [evaluation context] used for each set of rules
	# will be based on previous rules that may have been applied.
	my $current_element = shift;
	
	# shouldn't happen, but return 0 if it does.
	return 0 unless $current_element->nodeType == XML_ELEMENT_NODE;
	
	# The evaluation context.
	my %args = @_;
	my ($base, $parent_subject, $parent_object, $uri_mappings, $term_mappings,
		$incomplete_triples, $language, $graph, $xml_base);
		
	if ($args{'init'})
	{
		# At the beginning of processing, an initial [evaluation context] is created
		$base               = $self->uri;
		$parent_subject     = $base;
		$parent_object      = undef;
		$uri_mappings       = {};
		$term_mappings      = $self->{'options'}->{'keywords'};
		$incomplete_triples = ();
		$language           = undef;
		$graph              = $self->{'options'}->{'graph'} ? $self->{'options'}->{'graph_default'} : undef;
		$xml_base           = undef;
		
		if ($self->{'options'}->{'prefix_default'})
		{
			$uri_mappings->{'*'} = $self->{'options'}->{'prefix_default'};
		}
		
		if ($self->{'options'}->{'prefix_empty'})
		{
			$uri_mappings->{'-'} = $self->{'options'}->{'prefix_empty'};
		}
	}
	else
	{
		$base               = $args{'base'};
		$parent_subject     = $args{'parent_subject'};
		$parent_object      = $args{'parent_object'};
		$uri_mappings       = dclone($args{'uri_mappings'});
		$term_mappings      = dclone($args{'term_mappings'});
		$incomplete_triples = $args{'incomplete_triples'};
		$language           = $args{'language'};
		$graph              = $args{'graph'};
		$xml_base           = $args{'xml_base'};		
	}	
	
	# First, the local values are initialized
	my $recurse                  = 1;
	my $skip_element             = 0;
	my $new_subject              = undef;
	my $current_object_resource  = undef;
	my $local_uri_mappings       = $uri_mappings;
	my $local_term_mappings      = $term_mappings;
	my $local_incomplete_triples = ();
	my $current_language         = $language;
	
	my $activity = 0;

	# MOVED THIS SLIGHTLY EARLIER IN THE PROCESSING so that it can apply
	# to RDF/XML chunks.
	#
	# The [current element] is also parsed for any language information, and
	# if present, [current language] is set accordingly.
	# Language information can be provided using the general-purpose XML
	# attribute @xml:lang .
	if ($self->{'options'}->{'xhtml_lang'}
	&& $current_element->hasAttribute('lang'))
	{
		if ($self->_valid_lang( $current_element->getAttribute('lang') ))
		{
			$current_language = $current_element->getAttribute('lang');
		}
		else
		{
			$self->_log_error(
				ERR_WARNING,
				ERR_CODE_LANG_INVALID,
				sprintf('Language code "%s" is not valid.', $current_element->getAtrribute('lang')),
				element => $current_element,
				lang    => $current_element->getAttribute('lang'),
				) if $@;
		}
	}
	if ($self->{'options'}->{'xml_lang'}
	&& $current_element->hasAttributeNS(XML_XML_NS, 'lang'))
	{
		if ($self->_valid_lang( $current_element->getAttributeNS(XML_XML_NS, 'lang') ))
		{
			$current_language = $current_element->getAttributeNS(XML_XML_NS, 'lang');
		}
		else
		{
			$self->_log_error(
				ERR_WARNING,
				ERR_CODE_LANG_INVALID,
				sprintf('Language code "%s" is not valid.', $current_element->getAttributeNS(XML_XML_NS, 'lang')),
				element => $current_element,
				lang    => $current_element->getAttributeNS(XML_XML_NS, 'lang'),
				) if $@;
		}
	}

	# EXTENSION
	# xml:base - important for RDF/XML extension
	if ($current_element->hasAttributeNS(XML_XML_NS, 'base'))
	{
		$xml_base = $current_element->getAttributeNS(XML_XML_NS, 'base');
		$xml_base =~ s/#.*$//g;
		$xml_base = $self->uri($xml_base);
	}
	my $hrefsrc_base = $base;
	if ($self->{'options'}->{'xml_base'}==2 && defined $xml_base)
	{
		$hrefsrc_base = $xml_base;
	}

	# EXTENSION
	# Parses embedded RDF/XML - mostly useful for non-XHTML documents, e.g. SVG.
	if ($self->{'options'}->{'embedded_rdfxml'}
	&& $current_element->localname eq 'RDF'
	&& $current_element->namespaceURI eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#')
	{
		return 1 if $self->{'options'}->{'embedded_rdfxml'}==2;

		my $g = $self->bnode;
		
		my $fake_lang = 0;
		unless ($current_element->hasAttributeNS(XML_XML_NS, 'lang'))
		{
			$current_element->setAttributeNS(XML_XML_NS, 'lang', $current_language);
			$fake_lang = 1;
		}
		
		my $rdfxml_base = $self->{'origbase'};
		$rdfxml_base = $base
			if $self->{'options'}->{'xhtml_base'}==2;
		$rdfxml_base = $xml_base
			if defined $xml_base;
		
		eval {
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
						
						$self->_insert_triple_literal(undef, $s, $p, @o,
							($self->{'options'}->{'graph'} ? $g : undef));
					}
					else
					{
						$o[0] = $st->object->is_blank ?
							($g.'_'.$st->object->blank_identifier) :
							$st->object->uri_value ;
						$self->_insert_triple_resource(undef, $s, $p, @o,
							($self->{'options'}->{'graph'} ? $g : undef));
					}				
				});
		};
		
		$self->_log_error(
			ERR_WARNING,
			ERR_CODE_RDFXML_MESS,
			'Could not parse embedded RDF/XML content.',
			element => $current_element,
			) if $@;
		
		$current_element->removeAttributeNS(XML_XML_NS, 'lang')
			if ($fake_lang);
			
		return 1;
	}
	elsif ($current_element->localname eq 'RDF'
	and    $current_element->namespaceURI eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#')
	{
		$self->_log_error(
			ERR_WARNING,
			ERR_CODE_RDFXML_MUDDLE,
			'Encountered embedded RDF/XML content, but not configured to parse or skip it.',
			element => $current_element,
			);
	}

	
	# RDFa 1.1 - @profile
	if ($self->{'options'}->{'profiles'}
	and $current_element->hasAttribute('profile'))
	{
		my $profile = RDF::RDFa::Parser::Profile->new(
			$self->uri($current_element->getAttribute('profile')), $self);
		
		if (UNIVERSAL::isa($profile, 'RDF::RDFa::Parser::Profile'))
		{
			foreach my $mapping ($profile->get_prefixes)
			{
				$local_uri_mappings->{ $mapping->[0] } = $mapping->[1];				
			}
			foreach my $mapping ($profile->get_terms)
			{
				my ($term, $uri, $insensitive, $attrs) = @$mapping;
				$term = lc $term if $insensitive;
				$insensitive = $insensitive ? 'insensitive' : 'sensitive';
				$attrs = [ split /\s*/, ($attrs||'*') ];
				
				foreach my $attr (@$attrs)
				{
					$local_term_mappings->{$insensitive}->{$attr}->{$term} = $uri;
				}
			}
		}
		else
		{
			$self->_log_error(
				ERR_ERROR,
				ERR_CODE_PROFILE_UNUSABLE,
				sprintf("Unusable profile '%s'.", $current_element->getAttribute('profile')),
				uri     => $current_element->getAttribute('profile'),
				element => $current_element,
				);
		}
	}
	elsif ($current_element->hasAttribute('profile')
	and    $current_element->getAttribute('profile') ne 'http://www.w3.org/1999/xhtml/vocab')
	{
			$self->_log_error(
				ERR_WARNING,
				ERR_CODE_PROFILE_DISABLED,
				sprintf("Encountered profile '%s', but profiles are disabled.", $current_element->getAttribute('profile')),
				uri     => $current_element->getAttribute('profile'),
				element => $current_element,
				);
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
			my $pfx = $self->{'options'}->{'prefix_nocase'} ? (lc $1) : $1;
			my $uri = $A->getValue;
			if ($pfx =~ /^(xml|xmlns|_)$/i)
			{
				$self->_log_error(
					ERR_ERROR,
					ERR_CODE_PREFIX_BUILTIN,
					"Attempt to redefine built-in CURIE prefix '$pfx' not allowed.",
					element => $current_element,
					prefix  => $pfx,
					uri     => $uri,
					);
			}
			elsif ($pfx !~ /^($XML::RegExp::NCName)$/)
			{
				$self->_log_error(
					ERR_ERROR,
					ERR_CODE_PREFIX_ILLEGAL,
					"Attempt to define non-NCName CURIE prefix '$pfx' not allowed.",
					element => $current_element,
					prefix  => $pfx,
					uri     => $uri,
					);
			}
			elsif ($uri eq XML_XML_NS || $uri eq XML_XMLNS_NS)
			{
				$self->_log_error(
					ERR_ERROR,
					ERR_CODE_PREFIX_BUILTIN,
					"Attempt to define any CURIE prefix for '$uri' not allowed using \@xmlns.",
					element => $current_element,
					prefix  => $pfx,
					uri     => $uri,
					);
			}
			else
			{
				$self->{'sub'}->{'onprefix'}($self, $current_element, $pfx, $uri)
					if defined $self->{'sub'}->{'onprefix'};
				
				$local_uri_mappings->{$pfx} = $uri;
			}
		}
	}
	
	# RDFa 1.1 - @prefix support.
	# Note that this overwrites @xmlns:foo.
	if ($self->{'options'}->{'prefix_attr'}
	&& $current_element->hasAttribute('prefix'))
	{
		my $pfx_attr = $current_element->getAttribute('prefix');
		while ($pfx_attr =~ /^\s*(\S+):[\s\r\n]*(\S*)[\s\r\n]+/gs)
		{
			my $pfx = $self->{'options'}->{'prefix_nocase'} ? (lc $1) : $1;
			my $uri = $2;
			
			unless ($pfx =~ /^$XML::RegExp::NCName$/)
			{
				$self->_log_error(
					ERR_ERROR,
					ERR_CODE_PREFIX_ILLEGAL,
					"Attempt to define non-NCName CURIE prefix '$pfx' not allowed.",
					element => $current_element,
					prefix  => $pfx,
					uri     => $uri,
					);
				next;
			}
			
			$self->{'sub'}->{'onprefix'}($self, $current_element, $pfx, $uri)
				if defined $self->{'sub'}->{'onprefix'};
			$local_uri_mappings->{$pfx} = $uri;
		}
	}
	elsif ($current_element->hasAttribute('prefix'))
	{
		$self->_log_error(
			ERR_WARNING,
			ERR_CODE_PREFIX_DISABLED,
			"\@prefix found, but support disabled.",
			element => $current_element,
			);
	}
	
	# RDFa 1.1 - @vocab support
	if ($self->{'options'}->{'vocab_attr'}
	&& $current_element->hasAttribute('vocab'))
	{
		$local_uri_mappings->{'*'} = $self->uri(
			$current_element->getAttribute('vocab'));
	}
	elsif ($current_element->hasAttribute('vocab'))
	{
		$self->_log_error(
			ERR_WARNING,
			ERR_CODE_VOCAB_DISABLED,
			"\@vocab found, but support disabled.",
			element => $current_element,
			uri     => $self->uri(
				$current_element->getAttribute('vocab')),
			);
	}
	
	# EXTENSION
	# KjetilK's named graphs.
	if ($self->{'options'}->{'graph'})
	{
		my ($xmlns, $attr) = ($self->{'options'}->{'graph_attr'} =~ /^(?:\{(.+)\})?(.+)$/);
		unless ($attr)
		{
			$xmlns = undef;
			$attr  = 'graph';
		}
		
		if ($self->{'options'}->{'graph_type'} eq 'id'
		&&  $current_element->hasAttributeNS($xmlns, $attr))
		{
			$graph = $self->uri('#' . $current_element->getAttributeNS($xmlns, $attr));
		}
		elsif ($self->{'options'}->{'graph_type'} eq 'about'
		&&  $current_element->hasAttributeNS($xmlns, $attr))
		{
			$graph = $self->_expand_curie(
				$current_element->getAttributeNS($xmlns, $attr),
				element   => $current_element,
				attribute => 'graph',
				prefixes  => $local_uri_mappings,
				terms     => $local_term_mappings,
				xml_base  => $xml_base,
				);			
			$graph = $self->{'options'}->{'graph_default'}
				unless defined $graph;
		}
	}
	
	my @rel = $self->_split_tokens( $current_element->getAttribute('rel') );
	my @rev = $self->_split_tokens( $current_element->getAttribute('rev') );
	
	# EXTENSION: rel="alternate stylesheet"
	if ($self->{'options'}->{'alt_stylesheet'}
	&&  (grep /^alternate$/i, @rel)
	&&  (grep /^stylesheet$/i, @rel))
	{
		@rel = grep !/^(alternate|stylesheet)$/i, @rel;
		push @rel, ':ALTERNATE-STYLESHEET';
	}
	
	my @REL = map {
		my $x = $self->_expand_curie(
			$_,
			element   => $current_element,
			attribute => 'rel',
			prefixes  => $local_uri_mappings,
			terms     => $local_term_mappings,
			xml_base  => $xml_base,
			);
		defined $x ? ($x) : ();
		} @rel;	
	my @REV = map {
		my $x = $self->_expand_curie(
			$_,
			element   => $current_element,
			attribute => 'rev',
			prefixes  => $local_uri_mappings,
			terms     => $local_term_mappings,
			xml_base  => $xml_base,
			);
		defined $x ? ($x) : ();
		} @rev;
	
	# If the [current element] contains no valid @rel or @rev URI, obtained
	# according to the section on CURIE and URI Processing, then the next step 
	# is to establish a value for [new subject]. Any of the attributes that 
	# can carry a resource can set [new subject]
	unless ($current_element->hasAttribute('rel')
	||      $current_element->hasAttribute('rev'))
	{
		# [new subject] is set to the URI obtained from the first match
		# from the following rules:
		
		# by using the URI from @about, if present, obtained according to
		# the section on CURIE and URI Processing ; 
		if ($current_element->hasAttribute('about'))
		{
			$new_subject = $self->_expand_curie(
				$current_element->getAttribute('about'),
				element   => $current_element,
				attribute => 'about',
				prefixes  => $local_uri_mappings,
				terms     => $local_term_mappings,
				xml_base  => $xml_base,
				);
		}
			
		# otherwise, by using the URI from @src, if present, obtained
		# according to the section on CURIE and URI Processing.
		if ($current_element->hasAttribute('src') && !defined $new_subject)
		{
			$new_subject = $self->uri(
				$current_element->getAttribute('src'),
				{'element'=>$current_element,'xml_base'=>$hrefsrc_base}
				);
		}
			
		# otherwise , by using the URI from @resource, if present, obtained
		# according to the section on CURIE and URI Processing ; 
		if ($current_element->hasAttribute('resource') && !defined $new_subject)
		{
			$new_subject = $self->_expand_curie(
				$current_element->getAttribute('resource'), 
				element   => $current_element,
				attribute => 'resource',
				prefixes  => $local_uri_mappings,
				terms     => $local_term_mappings,
				xml_base  => $xml_base,
				);
		}
			
		# otherwise , by using the URI from @href, if present, obtained
		# according to the section on CURIE and URI Processing.
		if ($current_element->hasAttribute('href') && !defined $new_subject)
		{
			$new_subject = $self->uri(
				$current_element->getAttribute('href'),
				{'element'=>$current_element,'xml_base'=>$hrefsrc_base}
				);
		}
			
		# If no URI is provided by a resource attribute, then the first
		# match from the following rules will apply: 
		unless (defined $new_subject)
		{
			# if the element is the head or body element then act as if
			# there is an empty @about present, and process it according to
			# the rule for @about, above; 
			if ($self->{'options'}->{'xhtml_elements'}
			&& ($current_element->namespaceURI eq 'http://www.w3.org/1999/xhtml')
			&& ($current_element->tagName eq 'head' || $current_element->tagName eq 'body'))
			{
				$new_subject = $self->uri;
			}

			# EXTENSION: atom elements
			elsif ($self->{'options'}->{'atom_elements'}
			&& ($current_element->namespaceURI eq 'http://www.w3.org/2005/Atom')
			&& ($current_element->tagName eq 'feed' || $current_element->tagName eq 'entry'))
			{
				$new_subject = $self->_atom_magic($current_element);
			}

			# if @instanceof is present, obtained according to the section
			# on CURIE and URI Processing , then [new subject] is set to be
			# a newly created [bnode]; 
			elsif ($current_element->hasAttribute('typeof')
				||  $current_element->hasAttribute('instanceof'))
			{
				$new_subject = $self->bnode($current_element);
				
				if ($current_element->hasAttribute('instanceof')
				&& !$current_element->hasAttribute('typeof'))
				{
					$self->_log_error(
						ERR_WARNING,
						ERR_CODE_INSTANCEOF_USED,
						"Deprecated \@instanceof found; using it anyway.",
						element => $current_element,
						);
				}
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
		{
			$new_subject = $self->_expand_curie(
				$current_element->getAttribute('about'),
				element   => $current_element,
				attribute => 'about',
				prefixes  => $local_uri_mappings,
				terms     => $local_term_mappings,
				xml_base  => $xml_base,
				);
		}
			
		# otherwise, by using the URI from @src, if present, obtained
		# according to the section on CURIE and URI Processing.
		if ($current_element->hasAttribute('src') && !defined $new_subject)
		{
			$new_subject = $self->uri(
				$current_element->getAttribute('src'),
				{'element'=>$current_element,'xml_base'=>$hrefsrc_base}
				);
		}

		# If no URI is provided then the first match from the following rules
		# will apply: 
		unless (defined $new_subject)
		{
			# if the element is the head or body element then act as if
			# there is an empty @about present, and process it according
			# to the rule for @about, above; 
			if ($self->{'options'}->{'xhtml_elements'}
			&& ($current_element->namespaceURI eq 'http://www.w3.org/1999/xhtml')
			&& ($current_element->tagName eq 'head' || $current_element->tagName eq 'body'))
			{
				$new_subject = $self->uri;
			}
			
			# EXTENSION: atom elements
			elsif ($self->{'options'}->{'atom_elements'}
			&& ($current_element->namespaceURI eq 'http://www.w3.org/2005/Atom')
			&& ($current_element->tagName eq 'feed' || $current_element->tagName eq 'entry'))
			{
				$new_subject = $self->_atom_magic($current_element);
			}
			
			# if @instanceof is present, obtained according to the section
			# on CURIE and URI Processing, then [new subject] is set to be a
			# newly created [bnode]; 
			elsif ($current_element->hasAttribute('typeof')
				||  $current_element->hasAttribute('instanceof'))
			{
				$new_subject = $self->bnode($current_element);
				
				if ($current_element->hasAttribute('instanceof')
				&& !$current_element->hasAttribute('typeof'))
				{
					$self->_log_error(
						ERR_WARNING,
						ERR_CODE_INSTANCEOF_USED,
						"Deprecated \@instanceof found; using it anyway.",
						element => $current_element,
						);
				}
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
		{
			$current_object_resource = $self->_expand_curie(
				$current_element->getAttribute('resource'), 
				element   => $current_element,
				attribute => 'resource',
				prefixes  => $local_uri_mappings,
				terms     => $local_term_mappings,
				xml_base  => $xml_base,
				);
		}
			
		# otherwise, by using the URI from @href, if present, obtained according
		# to the section on CURIE and URI Processing.
		if ($current_element->hasAttribute('href') && !defined $current_object_resource)
		{
			$current_object_resource = $self->uri(
				$current_element->getAttribute('href'),
				{'element'=>$current_element,'xml_base'=>$hrefsrc_base}
				);
		}
		
		# Note that final value of the [current object resource] will either
		# be null (from initialization), a full URI or a bnode. 		
	}
	
	# If in any of the previous steps a [new subject] was set to a non-null
	# value, it is now used to provide a subject for type values
	if ($new_subject
	&& (  $current_element->hasAttribute('instanceof')
		|| $current_element->hasAttribute('typeof')))
	{

		if ($current_element->hasAttribute('instanceof')
		&&  $current_element->hasAttribute('typeof'))
		{
			$self->_log_error(
				ERR_WARNING,
				ERR_CODE_INSTANCEOF_OVERRULED,
				"Deprecated \@instanceof found; ignored because \@typeof also present.",
				element => $current_element,
				);
		}
		elsif ($current_element->hasAttribute('instanceof'))
		{
			$self->_log_error(
				ERR_WARNING,
				ERR_CODE_INSTANCEOF_USED,
				"Deprecated \@instanceof found; using it anyway.",
				element => $current_element,
				);
		}

		# One or more 'types' for the [ new subject ] can be set by using
		# @instanceof. If present, the attribute must contain one or more
		# URIs, obtained according to the section on URI and CURIE Processing...
	
		my @instanceof = $self->_split_tokens(  $current_element->getAttribute('typeof')
			|| $current_element->getAttribute('instanceof') );
		
		foreach my $curie (@instanceof)
		{
			my $rdftype = $self->_expand_curie(
				$curie,
				element   => $current_element,
				attribute => 'typeof',
				prefixes  => $local_uri_mappings,
				terms     => $local_term_mappings,
				xml_base  => $xml_base,
				);				
			next unless defined $rdftype;
		
			# ... each of which is used to generate a triple as follows:
			#
			# subject
			#     [new subject] 
			# predicate
	    	#     http://www.w3.org/1999/02/22-rdf-syntax-ns#type 
			# object
			#     full URI of 'type' 

			$self->_insert_triple_resource($current_element, $new_subject, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', $rdftype, $graph);
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
			$self->_insert_triple_resource($current_element, $new_subject, $r, $current_object_resource, $graph);
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
			$self->_insert_triple_resource($current_element, $current_object_resource, $r, $new_subject, $graph);
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
		
		push @$local_incomplete_triples,
			map {
				{
					predicate => $_,
					direction => 'forward',
					graph     => $graph
				};
			} @REL;
		
		#    * If present, @rev must contain one or more URIs, obtained
		#      according to the section on CURIE and URI Processing, each
		#      of which is added to the [local list of incomplete triples]
		#      as follows:
		#
		#      predicate
		#          full URI 
		#      direction
		#          reverse 
		
		push @$local_incomplete_triples,
			map {
				{
					predicate => $_,
					direction => 'reverse',
					graph     => $graph
				};
			} @REV;
		
		$current_object_resource = $self->bnode;
	}
	
	# The next step of the iteration is to establish any [current object
	# literal
	my @current_object_literal;
	
	my @prop = $self->_split_tokens( $current_element->getAttribute('property') );

	my $datatype = -1;
	if ($current_element->hasAttribute('datatype'))
	{
		$datatype = $self->_expand_curie(
			$current_element->getAttribute('datatype'),
			element   => $current_element,
			attribute => 'datatype',
			prefixes  => $local_uri_mappings,
			terms     => $local_term_mappings,
			xml_base  => $xml_base,
			);
	}
		
	if (@prop)
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
			@current_object_literal = ($self->_element_to_string($current_element),
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
			@current_object_literal = ($self->_element_to_xml($current_element, $current_language),
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
				@current_object_literal = ($self->_element_to_string($current_element),
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

		my $p = $self->_expand_curie(
			$property,
			element   => $current_element,
			attribute => 'property',
			prefixes  => $local_uri_mappings,
			terms     => $local_term_mappings,
			xml_base  => $xml_base,
			);
		next unless defined $p;
		
		$self->_insert_triple_literal($current_element, $new_subject, $p, @current_object_literal, $graph);
		$activity++;
		
		# Once the triple has been created, if the [datatype] of the
		# [current object literal] is rdf:XMLLiteral, then the [recurse]
		# flag is set to false.
		$recurse = 0
			if $datatype eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral';
	}

	# If the [recurse] flag is 'true', all elements that are children of the 
	# [current element] are processed using the rules described here, using a 
	# new [evaluation context], initialized as follows
	my $flag = 0;
	if ($recurse)
	{
		foreach my $kid ($current_element->getChildrenByTagName('*'))
		{
			# If the [skip element] flag is 'true' then the new [evaluation context]
			# is a copy of the current context that was passed in to this level of
			# processing, with the [language] and [list of URI mappings] values
			# replaced with the local values; 
			if ($skip_element)
			{
				$flag = $self->_consume_element($kid,
					base               => $base,
					parent_subject     => $parent_subject,
					parent_object      => $parent_object,
					uri_mappings       => $uri_mappings,
					term_mappings      => $term_mappings,
					incomplete_triples => $incomplete_triples,
					language           => $language,
					graph              => $graph,
					xml_base           => $xml_base,
				) || $flag;
			}
			
			# Otherwise, the values are: 
			else
			{
				$flag = $self->_consume_element($kid,
					base               => $base,
					parent_subject     => $new_subject,
					parent_object      => (defined $current_object_resource ? $current_object_resource : (defined $new_subject ? $new_subject : $parent_subject)),
					uri_mappings       => $local_uri_mappings,
					term_mappings      => $local_term_mappings,
					incomplete_triples => $local_incomplete_triples,
					language           => $current_language,
					graph              => $graph,
					xml_base           => $xml_base,
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
				$self->_insert_triple_resource($current_element, $parent_subject, $predicate, $new_subject, $parent_graph);
				$activity++;
			}
			else
			{
				$self->_insert_triple_resource($current_element, $new_subject, $predicate, $parent_subject, $parent_graph);
				$activity++;
			}
		}
	}

	return 1 if $activity || $new_subject || $flag;
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

For details of the callback functions, see the section CALLBACKS. If used, C<set_callbacks>
must be called I<before> C<consume>. C<set_callbacks> returns a reference to the parser
object itself.

=cut

sub set_callbacks
# Set callback functions for handling RDF triples.
{
	my $self = shift;

	if ('HASH' eq ref $_[0])
	{
		$self->{'sub'} = $_[0];
		$self->{'sub'}->{'pretriple_resource'} = \&_print0
			if lc $self->{'sub'}->{'pretriple_resource'} eq 'print';
		$self->{'sub'}->{'pretriple_literal'} = \&_print1
			if lc $self->{'sub'}->{'pretriple_literal'} eq 'print';
	}
	else
	{
		die "Unsupported set_callbacks call.\n";
	}
	
	return $self;
}

sub _print0
# Prints a Turtle triple.
{
	my $self    = shift;
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
	my $self    = shift;
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

sub _insert_triple_resource
{
	my $self = shift;

	my $suppress_triple = 0;
	$suppress_triple = $self->{'sub'}->{'pretriple_resource'}($self, @_)
		if defined $self->{'sub'}->{'pretriple_resource'};
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
	return $self->_insert_triple_common($element, $subject, $predicate, $to, $graph);
}

sub _insert_triple_literal
{
	my $self = shift;

	my $suppress_triple = 0;
	$suppress_triple = $self->{'sub'}->{'pretriple_literal'}($self, @_)
		if defined $self->{'sub'}->{'pretriple_literal'};
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
			if ($self->{'options'}->{'use_rtnlx'})
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
	$self->_insert_triple_common($element, $subject, $predicate, $to, $graph);
}

sub _insert_triple_common
{
	my $self      = shift;  # A reference to the RDF::RDFa::Parser object
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
	if ($self->{'options'}->{'graph'} && $graph)
	{
		$self->{Graphs}->{$graph}++;
		
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
	$suppress_triple = $self->{'sub'}->{'ontriple'}($self, $element, $statement)
		if ($self->{'sub'}->{'ontriple'});
	return if $suppress_triple;

	$self->{model}->add_statement($statement);
}

sub _atom_magic
{
	my $self    = shift;
	my $element = shift;
	
	return $self->bnode($element, 1);
}

# Splits things like property="foaf:name rdfs:label"
sub _split_tokens
{
	my ($self, $string) = @_;
	$string ||= '';
	$string =~ s/(^\s+|\s+$)//g;
	my @return = split /\s+/, $string;
	return @return;
}

sub _element_to_string
{
	my $self = shift;
	my $dom  = shift;
	
	if ($dom->nodeType == XML_TEXT_NODE)
	{
		return $dom->getData;
	}
	elsif ($dom->nodeType == XML_ELEMENT_NODE)
	{
		my $rv = '';
		foreach my $kid ($dom->childNodes)
			{ $rv .= $self->_element_to_string($kid); }
		return $rv;
	}

	return '';
}

sub _element_to_xml
{
	my $self = shift;
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
{
	my $self    = shift;
	my $element = shift;
	my $save_me = shift || 0;
	
	if (defined $element
	and $self->{'saved_bnodes'}->{ $element->nodePath })
	{
		return $self->{'saved_bnodes'}->{ $element->nodePath };
	}
	
	return sprintf('http://thing-described-by.org/?%s#%s',
		$self->uri,
		$self->{element}->getAttribute('id'))
		if ($self->{options}->{tdb_service} && $element && length $element->getAttribute('id'));

	my $rv = sprintf('_:RDFaAutoNode%03d', $self->{bnodes}++);
	
	if ($save_me and defined $element)
	{
		$self->{'saved_bnodes'}->{ $element->nodePath } = $rv;
	}
	
	return $rv;
}

sub _valid_lang
{
	my ($self, $value_to_test) = @_;

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

sub _expand_curie
{
	my ($self, $token, %args) = @_;	
	my $r = $self->__expand_curie($token, %args);
	
	if (defined $self->{'sub'}->{'oncurie'})
	{
		return $self->{'sub'}->{'oncurie'}($self, $args{element}, $token, $r);
	}

	return $r;
}

sub __expand_curie
{
	my ($self, $token, %args) = @_;

	# Blank nodes
	{
		my $bnode;
		if ($token eq '_:' || $token eq '[_:]')
			{ $bnode = '_:RDFaX'; }
		elsif ($token =~ /^_:RDFa(.*)$/i || $token =~ /^\[_:RDFa(.*)\]$/i)
			{ $bnode = '_:RDFax'.$1; }
		elsif ($token =~ /^_:(.+)$/i || $token =~ /^\[_:(.+)\]$/i)
			{ $bnode = '_:'.$1; }
		
		if (defined $bnode)
		{
			if ($args{'attribute'} =~ /^(rel|rev|property|datatype)$/i)
			{
				$self->_log_error(
					ERR_ERROR,
					ERR_CODE_BNODE_WRONGPLACE,
					"Blank node found in $args{attribute} where URIs are expected as values.",
					token     => $token,
					element   => $args{element},
					attribute => $args{attribute},
					);
			}

			return $bnode;
		}
	}
	
	my $is_safe = 0;
	if ($token =~ /^\[(.*)\]$/)
	{
		$is_safe = 1;
		$token   = $1;
	}
	
	# Keywords / terms / whatever-they're-called
	if ($token =~ /^($XML::RegExp::NCName)$/
	and ($is_safe || $args{'attribute'} =~ /^(rel|rev|property|typeof|datatype)$/i || $args{'allow_unsafe_term'}))
	{
		my $terms = $args{'terms'};
		my $attr  = $args{'attribute'};
		
		return $terms->{'sensitive'}->{$attr}->{$token}
			if defined $terms->{'sensitive'}->{ $attr }->{$token};
			
		return $terms->{'sensitive'}->{'*'}->{$token}
			if defined $terms->{'sensitive'}->{'*'}->{$token};
			
		return $terms->{'insensitive'}->{$attr}->{lc $token}
			if defined $terms->{'insensitive'}->{$attr}->{lc $token};
			
		return $terms->{'insensitive'}->{'*'}->{lc $token}
			if defined $terms->{'insensitive'}->{'*'}->{lc $token};
	}

	# CURIEs - prefixed
	if ($token =~ /^($XML::RegExp::NCName)?:(\S*)$/
	and ($is_safe || $args{'attribute'} =~ /^(rel|rev|property|typeof|datatype)$/i || $args{'allow_unsafe_curie'}))
	{
		$token =~ /^($XML::RegExp::NCName)?:(\S+)$/;
		my $prefix = (defined $1 && length $1) ? $1 : '-';
		my $suffix = $2;
		
		$prefix = lc $prefix if $self->{'options'}->{'prefix_nocase'};
		
		if (defined $args{'prefixes'}->{$prefix})
		{
			return $args{'prefixes'}->{$prefix} . $suffix;
		}

		if ($is_safe)
		{
			$self->_log_error(
				ERR_ERROR,
				ERR_CODE_CURIE_UNDEFINED,
				"CURIE '$token' used in safe CURIE, but '$prefix' is undefined.",
				token     => $token,
				element   => $args{element},
				attribute => $args{attribute},
				prefix    => $prefix,
				);
			return undef;
		}
	}

	# CURIEs - bare prefixes
	if ($self->{'options'}->{'prefix_bare'}
	and $token =~ /^($XML::RegExp::NCName)$/
	and ($is_safe || $args{'attribute'} =~ /^(rel|rev|property|typeof|datatype)$/i || $args{'allow_unsafe_curie'}))
	{
		my $prefix = $token;
		my $suffix = '';
		
		$prefix = lc $prefix if $self->{'options'}->{'prefix_nocase'};
		
		if (defined $args{'prefixes'}->{$prefix})
		{
			return $args{'prefixes'}->{$prefix} . $suffix;
		}
	}

	# Absolute URIs
	if ($token =~ /^[A-Z][A-Z0-9\.\+-]*:/i and !$is_safe
	and ($self->{'options'}->{'full_uris'} || $args{'attribute'} =~ /^(about|resource|graph)$/i))
	{
		return $token;
	}

	# CURIEs - default vocab
	if ($token =~ /^($XML::RegExp::NCName)$/
	and ($is_safe || $args{'attribute'} =~ /^(rel|rev|property|typeof|datatype)$/i || $args{'allow_unsafe_default_vocab'}))
	{
		my $prefix = '*';
		my $suffix = $token;
		
		if (defined $args{'prefixes'}->{$prefix})
		{
			return $args{'prefixes'}->{$prefix} . $suffix;
		}
	
		return undef if $is_safe;
	}

	# Relative URIs
	if (!$is_safe and ($args{'attribute'} =~ /^(about|resource|graph)$/i || $args{'allow_relative'}))
	{
		return $self->uri($token, {'element'=>$args{'element'}, 'xml_base'=>$args{'xml_base'}});
	}
	
	$self->_log_error(
		ERR_WARNING,
		ERR_CODE_CURIE_FELLTHROUGH,
		"Couldn't make sense of token '$token'.",
		token     => $token,
		element   => $args{element},
		attribute => $args{attribute},
		);

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
		my $i = $self->{model}->get_statements(undef, undef, undef, $tg);
		while (my $statement = $i->next)
		{
			$m->add_statement($statement);
		}
		return $m;
	}
	else
	{
		return $self->{model};
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
	my $self = shift;
	return $self->{dom};
}

=item C<< $p->xhtml >>

Returns the XHTML/XML source of the document being parsed.

=cut

sub xhtml
{
	my $self = shift;
	return $self->{xhtml};
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
	my $self  = shift;
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
	
	my $base = $self->{baseuri};
	if ($self->{'options'}->{'xml_base'})
	{
		$base = $opts->{'xml_base'} || $self->{baseuri};
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

=item C<< $p->errors >>

Returns a list of errors and warnings that occurred during parsing.

=cut

sub errors
{
	my $self = shift;
	return @{$self->{errors}};
}

sub _log_error
{
	my ($self, $level, $code, $message, %args) = @_;
	
	if (defined $self->{'sub'}->{'onerror'})
	{
		$self->{'sub'}->{'onerror'}(@_);
	}
	elsif ($level eq ERR_ERROR)
	{
		warn "${code}: ${message}\n";
		warn sprintf("... with URI <%s>\n",
			$args{'uri'})
			if defined $args{'uri'};
		warn sprintf("... on element '%s' with path '%s'\n",
			$args{'element'}->localname,
			$args{'element'}->nodePath)
			if UNIVERSAL::isa($args{'element'}, 'XML::LibXML::Node');
	}
	
	push @{$self->{errors}}, [$level, $code, $message, \%args];
}

sub OPTS_XHTML
{
	warn "OPTS_XHTML is deprecated.\n";
	return RDF::RDFa::Parser::Config->new(
		RDF::RDFa::Parser::Config->HOST_XHTML,
		RDF::RDFa::Parser::Config->RDFA_10,
		@_);
}
sub OPTS_XHTML_11
{
	warn "OPTS_XHTML_11 is deprecated.\n";
	return RDF::RDFa::Parser::Config->new(
		RDF::RDFa::Parser::Config->HOST_XHTML,
		RDF::RDFa::Parser::Config->RDFA_11,
		@_);
}
sub OPTS_HTML4
{
	warn "OPTS_HTML4 is deprecated.\n";
	return RDF::RDFa::Parser::Config->new(
		RDF::RDFa::Parser::Config->HOST_HTML4,
		RDF::RDFa::Parser::Config->RDFA_11,
		@_);
}
sub OPTS_HTML5
{
	warn "OPTS_HTML5 is deprecated.\n";
	return RDF::RDFa::Parser::Config->new(
		RDF::RDFa::Parser::Config->HOST_HTML5,
		RDF::RDFa::Parser::Config->RDFA_11,
		@_);
}
sub OPTS_SVG
{
	warn "OPTS_SVG is deprecated.\n";
	return RDF::RDFa::Parser::Config->new(
		RDF::RDFa::Parser::Config->HOST_SVG,
		RDF::RDFa::Parser::Config->RDFA_10,
		@_);
}
sub OPTS_ATOM
{
	warn "OPTS_ATOM is deprecated.\n";
	return RDF::RDFa::Parser::Config->new(
		RDF::RDFa::Parser::Config->HOST_ATOM,
		RDF::RDFa::Parser::Config->RDFA_10,
		@_);
}
sub OPTS_XML
{
	warn "OPTS_XML is deprecated.\n";
	return RDF::RDFa::Parser::Config->new(
		RDF::RDFa::Parser::Config->HOST_XML,
		RDF::RDFa::Parser::Config->RDFA_10,
		@_);
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

=head2 ontoken

This is called when a CURIE has been expanded. The parameters are:

=over 4

=item * A reference to the C<RDF::RDFa::Parser> object

=item * A reference to the C<XML::LibXML::Element> being parsed

=item * The CURIE or token as a string (e.g. "foaf:name" or "Stylesheet")

=item * The fully expanded URI

=back

The callback function must return a fully expanded URI, or if it
wants the CURIE to be ignored, undef.

=head2 onerror

This is called when an error occurs:

=over 4

=item * A reference to the C<RDF::RDFa::Parser> object

=item * The error level (RDF::RDFa::Parser::ERR_ERROR or
RDF::RDFa::Parser::ERR_WARNING)

=item * An error code

=item * An error message

=item * A hash of other information

=back

The return value of this callback is currently ignored, but you should return
0 in case future versions of this module assign significance to the return value.

If you do not define an onerror callback, then errors will be output via STDERR
and warnings will be silent. Either way, you can retrieve errors after parsing
using the C<errors> method.

=head1 FEATURES

=head2 HTML Support

The constructor method parses incoming markup as well-formed XML and will
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
  
  my $config      = RDF::RDFa::Parser::Config->new(
                      RDF::RDFa::Parser::Config->HOST_HTML5,
                      RDF::RDFa::Parser::Config->RDFA_11);
  my $rdfa_parser = RDF::RDFa::Parser->new(
                      $fixed_doc,
                      'http://example.com/doc.html',
                      $config);

C<RDF::RDFa::Parser::Config> is capable of enabling 
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
in assigning blank node identifiers consistently, etc. Callbacks I<should> work properly,
but this has not been extensively tested. If XML::Atom::OWL is not installed, then this
option will be silently ignored.

C<RDF::RDFa::Parser::Config> is capable of enabling 
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

C<RDF::RDFa::Parser::Config> is capable of enabling 
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

L<RDF::RDFa::Parser::Config>, L<RDF::RDFa::Parser::Profile>. 

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
