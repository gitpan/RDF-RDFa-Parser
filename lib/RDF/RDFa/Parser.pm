package RDF::RDFa::Parser;

=head1 NAME

RDF::RDFa::Parser - flexible RDFa parser

=head1 SYNOPSIS

 use RDF::RDFa::Parser;
 
 ### Create an object...
 $p = RDF::RDFa::Parser->new_from_url($url);
 # or: $p = RDF::RDFa::Parser->new($markup, $base_url);
 
 ### Get an RDF::Trine::Model containing the document's data...
 $data = $p->graph;
 
 ### Get Open Graph Protocol data...
 $title = $p->opengraph('title');

=cut

use Data::UUID;
use File::ShareDir qw(dist_file);
use HTML::HTML5::Parser;
use HTML::HTML5::Sanity qw(fix_document);
use LWP::UserAgent;
use RDF::RDFa::Parser::Config;
use RDF::RDFa::Parser::OpenDocumentObjectModel;
use RDF::RDFa::Parser::Profile;
use RDF::Trine 0.123;
use Scalar::Util qw(blessed);
use Storable qw(dclone);
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
use constant {
	RDF_XMLLIT   => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral',
	RDF_TYPE     => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
	};
use common::sense;
use 5.008;

=head1 VERSION

1.092

=cut

our $VERSION = '1.092';
our $HAS_AWOL;

BEGIN
{
	local $@;
	eval "use XML::Atom::OWL;";
	$HAS_AWOL = $@ ? 0 : 1;
}

=head1 DESCRIPTION

=head2 Constructors

=over 4

=item C<< $p = RDF::RDFa::Parser->new($markup, $base, [$config], [$storage]) >>

This method creates a new RDF::RDFa::Parser object and returns it.

The $markup variable may contain an XHTML/XML string, or a
XML::LibXML::Document. If a string, the document is parsed using
XML::LibXML::Parser or HTML::HTML5::Parser, depending on the
configuration in $config. XML well-formedness errors will cause the
function to die.

$base is a URL used to resolve relative links found in the document.

$config optionally holds an RDF::RDFa::Parser::Config object which
determines the set of rules used to parse the RDFa. It defaults to
XHTML+RDFa 1.1.

B<Advanced usage note:> $storage optionally holds an RDF::Trine::Store
object. If undef, then a new temporary store is created.

=cut

sub new
{
	my ($class, $markup, $base_uri, $config, $store)= @_;
	
	# Rationalise $config
	# ===================
	# If $config is undefined, then use the default configuration
	if (!defined $config)
		{ $config = RDF::RDFa::Parser::Config->new; }
	# If $config is something sensible, then use it.
	elsif (blessed($config) && $config->isa('RDF::RDFa::Parser::Config'))
		{ 1; }
	# If it's a hashref (for backcompat), then use default plus those options
	elsif ('HASH' eq ref $config)
		{ $config = RDF::RDFa::Parser::Config->new(undef, undef, %$config); }
	# If it's something odd, then bail.
	else
		{ die "Unrecognised configuration\n"; }

	# Rationalise $base_uri
	# =====================
	unless ($base_uri =~ /^[a-z][a-z0-9\+\-\.]*:/i)
		{ die "Need a valid base URI.\n"; }

	# Rationalise $markup and set $dom
	# ================================
	my $dom;
	if (!defined $markup)
	{
		die "Need to provide markup to parse.";
	}
	elsif (blessed($markup) && $markup->isa('XML::LibXML::Document'))
	{
		$dom    = $markup;
		$markup = $dom->toString;
	}
	elsif ($config->{'dom_parser'} =~ /^(opendocument|opendoc|odf|od)$/i)
	{
		my $parser = RDF::RDFa::Parser::OpenDocumentObjectModel->new;
		$dom = $parser->parse_string($markup, $base_uri);
	}
	elsif ($config->{'dom_parser'} =~ /^(html|tagsoup|soup)$/i)
	{
		my $parser = HTML::HTML5::Parser->new;
		$dom = fix_document( $parser->parse_string($markup) );
	}
	else
	{
		my $parser  = XML::LibXML->new;
		
		my $catalogue = dist_file('RDF-RDFa-Parser', 'catalogue/index.xml');
		$parser->load_catalog($catalogue)
			if -r $catalogue;
		$parser->validation(0);
		$parser->recover(1);
		
		$dom = $parser->parse_string($markup);
	}

	# Rationalise $store
	# ==================
	$store = RDF::Trine::Store::Memory->temporary_store
		unless defined $store;

	my $self = bless {
		'baseuri'  => $base_uri,
		'origbase' => $base_uri,
		'dom'      => $dom,
		'model'    => RDF::Trine::Model->new($store),
		'bnodes'   => 0,
		'sub'      => {},
		'options'  => $config,
		'Graphs'   => {},
		'errors'   => [],
		'consumed' => 0,
		}, $class;
	
	$config->auto_config($self);
	
	$self->{options} = $config = $config->guess_rdfa_version($self)
		if $config->{guess_rdfa_version};

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

=item C<< $p = RDF::RDFa::Parser->new_from_url($url, [$config], [$storage]) >>

$url is a URL to fetch and parse.

$config optionally holds an RDF::RDFa::Parser::Config object which
determines the set of rules used to parse the RDFa. The default is
to determine the configuration by looking at the HTTP response
Content-Type header; it's probably sensible to keep the default.

$storage optionally holds an RDF::Trine::Store object. If undef, then
a new temporary store is created.

This function can also be called as C<new_from_uri>. Same thing.

=cut

sub new_from_url
{
	my ($class, $url, $config, $store)= @_;
	
	my $response = do
		{
			if (blessed($url) && $url->isa('HTTP::Message'))
				{ $url; }
			else
				{ $config->lwp_ua->get($url);	}
		};
	my $host = $response->content_type;
	
	if (blessed($config) && $config->isa('RDF::RDFa::Parser::Config'))
		{ $config = $config->rehost($host); }
	elsif (ref $config eq 'HASH')
		{ $config = RDF::RDFa::Parser::Config->new($host, undef, %$config); }
	else
		{ $config = RDF::RDFa::Parser::Config->new($host); }

	return $class->new(
		$response->decoded_content,
		$response->base.'',
		$config,
		$store,
		);
}

*new_from_uri = \&new_from_url;

=item C<< $p = RDF::RDFa::Parser->new_from_response($response, [$config], [$storage]) >>

$response is an C<HTTP::Response> object.

Otherwise the same as C<new_from_url>. 

=cut

*new_from_response = \&new_from_url;

=back

=head2 Public Methods

=over 4

=item C<< $p->graph  >>

This will return an RDF::Trine::Model containing all the RDFa
data found on the page.

B<Advanced usage note:> If passed a graph URI as a parameter,
will return a single named graph from within the page. This
feature is only useful if you're using named graphs.

=cut

sub graph
{
	my $self  = shift;
	my $graph = shift;
	
	$self->consume;
	
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
		my $m = RDF::Trine::Model->temporary_model;
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

B<Advanced usage only.>

Will return a hashref of all named graphs, where the graph name is a
key and the value is a RDF::Trine::Model tied to a temporary storage.

This method is only useful if you're using named graphs.

=cut

sub graphs
{
	my $self = shift;
	$self->consume;
	
	my @graphs = keys(%{$self->{Graphs}});
	my %result;
	foreach my $graph (@graphs)
	{
		$result{$graph} = $self->graph($graph);
	}
	return \%result;
}

=item C<< $p->opengraph([$property])  >>

If $property is provided, will return the value or list of values (if
called in list context) for that Open Graph Protocol property. (In pure
RDF terms, it returns the non-bnode objects of triples where the
subject is the document base URI; and the predicate is $property,
with non-URI $property strings taken as having the implicit prefix
'http://ogp.me/ns#'. There is no distinction between literal and
non-literal values.)

If $property is omitted, returns a list of possible properties.

Example:

  foreach my $property (sort $p->opengraph)
  {
    print "$property :\n";
    foreach my $val (sort $p->opengraph($property))
    {
      print "  * $val\n";
    }
  }

See also: L<http://opengraphprotocol.org/>.

=cut

sub opengraph
{
	my $self = shift;
	$self->consume;
	
	my $property = shift;
	$property = $1
		if defined $property && $property =~ m'^http://opengraphprotocol\.org/schema/(.*)$';
	$property = $1
		if defined $property && $property =~ m'^http://ogp\.me/ns#(.*)$';
		
	my $rtp;
	if (defined $property && $property =~ /^[a-z][a-z0-9\-\.\+]*:/i)
		{ $rtp = RDF::Trine::Node::Resource->new($property); }
	elsif (defined $property)
		{ $rtp = RDF::Trine::Node::Resource->new('http://ogp.me/ns#'.$property); }
		
	my $iter = $self->graph->get_statements(
		RDF::Trine::Node::Resource->new($self->uri), $rtp, undef);
	my $data = {};
	while (my $st = $iter->next)
	{
		my $propkey = $st->predicate->uri;
		$propkey = $1
			if $propkey =~ m'^http://ogp\.me/ns#(.*)$'
			|| $propkey =~ m'^http://opengraphprotocol\.org/schema/(.*)$';
		
		if ($st->object->is_resource)
			{ push @{ $data->{$propkey} }, $st->object->uri; }	
		elsif ($st->object->is_literal)
			{ push @{ $data->{$propkey} }, $st->object->literal_value; }
	}
	
	my @return;
	if (defined $property)
		{ @return = @{$data->{$property}} if defined $data->{$property}; }
	else
		{ @return = keys %$data; }
	
	return (wantarray ? @return : $return[0]);	
}

=item C<< $p->dom >>

Returns the parsed XML::LibXML::Document.

=cut

sub dom
{
	my $self = shift;
	return $self->{dom};
}

sub xhtml
{
	my $self = shift;
	warn "The ->xhtml method is deprecated. Use ->dom->toString instead.\n";
	return $self->dom->toString;
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
		warn sprintf("%04X: %s\n", $code, $message);
		warn sprintf("... with URI <%s>\n",
			$args{'uri'})
			if defined $args{'uri'};
		warn sprintf("... on element '%s' with path '%s'\n",
			$args{'element'}->localname,
			$args{'element'}->nodePath)
			if blessed($args{'element'}) && $args{'element'}->isa('XML::LibXML::Node');
	}
	
	push @{$self->{errors}}, [$level, $code, $message, \%args];
}

=item C<< $p->consume >>

B<Advanced usage only.>

The document is parsed for RDFa. As of RDF::RDFa::Parser 1.09x,
this is called automatically when needed; you probably don't need
to touch it unless you're doing interesting things with callbacks.

=cut

sub consume
{
	my $self = shift;
	
	return if $self->{'consumed'};
	$self->{'consumed'}++;
		
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
		$term_mappings      = {};
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

	# Used by OpenDocument, otherwise usually undef.
	my $rdfans = $self->{'options'}->{'ns'} || undef;

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
	&& $current_element->hasAttributeNsSafe(XML_XML_NS, 'lang'))
	{
		if ($self->_valid_lang( $current_element->getAttributeNsSafe(XML_XML_NS, 'lang') ))
		{
			$current_language = $current_element->getAttributeNsSafe(XML_XML_NS, 'lang');
		}
		else
		{
			$self->_log_error(
				ERR_WARNING,
				ERR_CODE_LANG_INVALID,
				sprintf('Language code "%s" is not valid.', $current_element->getAttributeNsSafe(XML_XML_NS, 'lang')),
				element => $current_element,
				lang    => $current_element->getAttributeNsSafe(XML_XML_NS, 'lang'),
				) if $@;
		}
	}

	# EXTENSION
	# xml:base - important for RDF/XML extension
	if ($current_element->hasAttributeNsSafe(XML_XML_NS, 'base'))
	{
		$xml_base = $current_element->getAttributeNsSafe(XML_XML_NS, 'base');
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

		my $g = $graph;
		unless ($self->{'options'}->{'embedded_rdfxml'} == 3)
		{
			$g = $self->bnode;
		}
		
		my $fake_lang = 0;
		unless ($current_element->hasAttributeNsSafe(XML_XML_NS, 'lang'))
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

	my @profiles;

	# RDFa 1.1 - @profile
	if ($self->{'options'}->{'profiles'}
	and $current_element->hasAttributeNsSafe($rdfans, 'profile'))
	{
		push @profiles, $self->_split_tokens( $current_element->getAttributeNsSafe($rdfans, 'profile') );		
	}
	elsif ($current_element->hasAttributeNsSafe($rdfans, 'profile')
	and    $current_element->getAttributeNsSafe($rdfans, 'profile') ne 'http://www.w3.org/1999/xhtml/vocab')
	{
		$self->_log_error(
			ERR_WARNING,
			ERR_CODE_PROFILE_DISABLED,
			sprintf("Encountered profile '%s', but profiles are disabled.", $current_element->getAttributeNsSafe($rdfans, 'profile')),
			uri     => $current_element->getAttributeNsSafe($rdfans, 'profile'),
			element => $current_element,
			);
	}

	# Various weird/default profile features.
	if ($current_element->isSameNode($self->dom->documentElement))
	{
		if ($self->{'options'}->{'profile_pi'})
		{
			foreach my $node ($self->dom->childNodes)
			{
				next unless $node->nodeType == XML_PI_NODE;
				next unless $node->nodeName == 'profile';
				my $uri = $node->getData;
				$uri =~ s/(^\s+|\s+$)//g;
				push @profiles, $uri;
			}
		}
		
		if ($self->{'options'}->{'default_profiles'})
		{
			push @profiles, $self->_split_tokens( $self->{'options'}->{'default_profiles'} );
		}		
	}

	foreach my $uri (reverse @profiles)
	{
		next unless $uri =~ /\S/; # skip duds
		
		my $profile = RDF::RDFa::Parser::Profile->new(
			$self->uri($uri, {'element'=>$current_element,'xml_base'=>$xml_base}),
			$self);
			
		if (blessed($profile) && $profile->isa('RDF::RDFa::Parser::Profile'))
		{
			foreach my $mapping ($profile->get_prefixes)
			{
				my ($prefix, $uri, $insensitive) = @$mapping;
				$prefix = lc $prefix if $insensitive;
				$insensitive = $insensitive ? 'insensitive' : 'sensitive';
				$local_uri_mappings->{$insensitive}->{$prefix} = $uri;				
			}
			foreach my $mapping ($profile->get_terms)
			{
				my ($term, $uri, $insensitive, $attrs) = @$mapping;
				$term = lc $term if $insensitive;
				$insensitive = $insensitive ? 'insensitive' : 'sensitive';
				$attrs = [ split /\s+/, ($attrs||'*') ];
				
				foreach my $attr (@$attrs)
				{
					$local_term_mappings->{$insensitive}->{$attr}->{$term} = $uri;
				}
			}
			my $profile_default_vocab = $profile->get_vocabulary;
			$local_uri_mappings->{'*'} = $profile_default_vocab
				if defined $profile_default_vocab;
		}
		else
		{
			$self->_log_error(
				ERR_ERROR,
				ERR_CODE_PROFILE_UNUSABLE,
				sprintf("Unusable profile '%s'.", $current_element->getAttributeNsSafe($rdfans, 'profile')),
				uri     => $current_element->getAttribute('profile'),
				element => $current_element,
				);
			return 0; # TODO: check this does what I want it to!
		}
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
	if ($self->{'options'}->{'xmlns_attr'})
	{
		foreach my $A ($current_element->getAttributes)
		{
			my $attr = $A->getName;
			
			if ($attr =~ /^xmlns\:(.+)$/i)
			{
				my $pfx = $self->{'options'}->{'prefix_nocase_xmlns'} ? (lc $1) : $1;
				my $cls = $self->{'options'}->{'prefix_nocase_xmlns'} ? 'insensitive' : 'sensitive';
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
					$self->{'sub'}->{'onprefix'}($self, $current_element, $pfx, $uri, $cls)
						if defined $self->{'sub'}->{'onprefix'};
					
					$local_uri_mappings->{$cls}->{$pfx} = $uri;
				}
			}
		}
	}
	
	# RDFa 1.1 - @prefix support.
	# Note that this overwrites @xmlns:foo.
	if ($self->{'options'}->{'prefix_attr'}
	&& $current_element->hasAttributeNsSafe($rdfans, 'prefix'))
	{
		my $pfx_attr = $current_element->getAttributeNsSafe($rdfans, 'prefix') . ' ';
		while ($pfx_attr =~ /^\s*(\S+):[\s\r\n]*(\S*)[\s\r\n]+/gs)
		{
			my $pfx = $self->{'options'}->{'prefix_nocase_attr'} ? (lc $1) : $1;
			my $cls = $self->{'options'}->{'prefix_nocase_attr'} ? 'insensitive' : 'sensitive';
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
			
			$self->{'sub'}->{'onprefix'}($self, $current_element, $pfx, $uri, $cls)
				if defined $self->{'sub'}->{'onprefix'};
			$local_uri_mappings->{$cls}->{$pfx} = $uri;
		}
	}
	elsif ($current_element->hasAttributeNsSafe($rdfans, 'prefix'))
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
	&& $current_element->hasAttributeNsSafe($rdfans, 'vocab'))
	{
		$local_uri_mappings->{'*'} = $self->uri(
			$current_element->getAttributeNsSafe($rdfans, 'vocab'),
			{'element'=>$current_element,'xml_base'=>$xml_base});
	}
	elsif ($current_element->hasAttributeNsSafe($rdfans, 'vocab'))
	{
		$self->_log_error(
			ERR_WARNING,
			ERR_CODE_VOCAB_DISABLED,
			"\@vocab found, but support disabled.",
			element => $current_element,
			uri     => $self->uri(
				$current_element->getAttributeNsSafe($rdfans, 'vocab'),
				{'element'=>$current_element,'xml_base'=>$xml_base}),
			);
	}
	
	# EXTENSION
	# KjetilK's named graphs.
	if ($self->{'options'}->{'graph'})
	{
		my ($xmlns, $attr) = ($self->{'options'}->{'graph_attr'} =~ /^(?:\{(.+)\})?(.+)$/);
		unless ($attr)
		{
			$xmlns = $rdfans;
			$attr  = 'graph';
		}
		
		if ($self->{'options'}->{'graph_type'} eq 'id'
		&&  $current_element->hasAttributeNsSafe($xmlns, $attr))
		{
			$graph = $self->uri('#' . $current_element->getAttributeNsSafe($xmlns, $attr));
		}
		elsif ($self->{'options'}->{'graph_type'} eq 'about'
		&&  $current_element->hasAttributeNsSafe($xmlns, $attr))
		{
			$graph = $self->_expand_curie(
				$current_element->getAttributeNsSafe($xmlns, $attr),
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

	# EXTENSION: @role
	if ($self->{'options'}->{'role_attr'}
	&&  $current_element->hasAttributeNsSafe($rdfans, 'role'))
	{
		my @role = $self->_split_tokens( $current_element->getAttributeNsSafe($rdfans, 'role') );
		my @ROLE = map {
			my $x = $self->_expand_curie(
				$_,
				element   => $current_element,
				attribute => 'role',
				prefixes  => $local_uri_mappings,
				terms     => $local_term_mappings,
				xml_base  => $xml_base,
				);
			defined $x ? ($x) : ();
			} @role;	
		if (@ROLE)
		{
			my $element_subject;
			if ($current_element->hasAttribute('id'))
			{
				$element_subject = $self->uri(sprintf('#%s',
					$current_element->getAttribute('id')),
					{'element'=>$current_element,'xml_base'=>$xml_base});
			}
			else
			{
				$element_subject = $self->bnode;
			}
			
			foreach my $r (@ROLE)
			{
				$self->_insert_triple_resource($current_element, $element_subject, 'http://www.w3.org/1999/xhtml/vocab#role', $r, $graph);
			}
		}
	}
	
	my @rel = $self->_split_tokens( $current_element->getAttributeNsSafe($rdfans, 'rel') );
	my @rev = $self->_split_tokens( $current_element->getAttributeNsSafe($rdfans, 'rev') );

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
	unless ($current_element->hasAttributeNsSafe($rdfans, 'rel')
	||      $current_element->hasAttributeNsSafe($rdfans, 'rev'))
	{
		# [new subject] is set to the URI obtained from the first match
		# from the following rules:
		
		# by using the URI from @about, if present, obtained according to
		# the section on CURIE and URI Processing ; 
		if ($current_element->hasAttributeNsSafe($rdfans, 'about'))
		{
			$new_subject = $self->_expand_curie(
				$current_element->getAttributeNsSafe($rdfans, 'about'),
				element   => $current_element,
				attribute => 'about',
				prefixes  => $local_uri_mappings,
				terms     => $local_term_mappings,
				xml_base  => $xml_base,
				);
		}
			
		# otherwise, by using the URI from @src, if present, obtained
		# according to the section on CURIE and URI Processing.
		if ($current_element->hasAttributeNsSafe($rdfans, 'src') && !defined $new_subject)
		{
			$new_subject = $self->uri(
				$current_element->getAttributeNsSafe($rdfans, 'src'),
				{'element'=>$current_element,'xml_base'=>$hrefsrc_base}
				);
		}
			
		# otherwise , by using the URI from @resource, if present, obtained
		# according to the section on CURIE and URI Processing ; 
		if ($current_element->hasAttributeNsSafe($rdfans, 'resource') && !defined $new_subject)
		{
			$new_subject = $self->_expand_curie(
				$current_element->getAttributeNsSafe($rdfans, 'resource'), 
				element   => $current_element,
				attribute => 'resource',
				prefixes  => $local_uri_mappings,
				terms     => $local_term_mappings,
				xml_base  => $xml_base,
				);
		}
			
		# otherwise , by using the URI from @href, if present, obtained
		# according to the section on CURIE and URI Processing.
		if ($current_element->hasAttributeNsSafe($rdfans, 'href') && !defined $new_subject)
		{
			$new_subject = $self->uri(
				$current_element->getAttributeNsSafe($rdfans, 'href'),
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
			elsif ($current_element->hasAttributeNsSafe($rdfans, 'typeof')
				||  $current_element->hasAttributeNsSafe($rdfans, 'instanceof'))
			{
				$new_subject = $self->bnode($current_element);
				
				if ($current_element->hasAttributeNsSafe($rdfans, 'instanceof')
				&& !$current_element->hasAttributeNsSafe($rdfans, 'typeof'))
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
					unless $current_element->hasAttributeNsSafe($rdfans, 'property');
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
		if ($current_element->hasAttributeNsSafe($rdfans, 'about'))
		{
			$new_subject = $self->_expand_curie(
				$current_element->getAttributeNsSafe($rdfans, 'about'),
				element   => $current_element,
				attribute => 'about',
				prefixes  => $local_uri_mappings,
				terms     => $local_term_mappings,
				xml_base  => $xml_base,
				);
		}
			
		# otherwise, by using the URI from @src, if present, obtained
		# according to the section on CURIE and URI Processing.
		if ($current_element->hasAttributeNsSafe($rdfans, 'src') && !defined $new_subject)
		{
			$new_subject = $self->uri(
				$current_element->getAttributeNsSafe($rdfans, 'src'),
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
			elsif ($current_element->hasAttributeNsSafe($rdfans, 'typeof')
				||  $current_element->hasAttributeNsSafe($rdfans, 'instanceof'))
			{
				$new_subject = $self->bnode($current_element);
				
				if ($current_element->hasAttributeNsSafe($rdfans, 'instanceof')
				&& !$current_element->hasAttributeNsSafe($rdfans, 'typeof'))
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
		if ($current_element->hasAttributeNsSafe($rdfans, 'resource'))
		{
			$current_object_resource = $self->_expand_curie(
				$current_element->getAttributeNsSafe($rdfans, 'resource'), 
				element   => $current_element,
				attribute => 'resource',
				prefixes  => $local_uri_mappings,
				terms     => $local_term_mappings,
				xml_base  => $xml_base,
				);
		}
			
		# otherwise, by using the URI from @href, if present, obtained according
		# to the section on CURIE and URI Processing.
		if ($current_element->hasAttributeNsSafe($rdfans, 'href') && !defined $current_object_resource)
		{
			$current_object_resource = $self->uri(
				$current_element->getAttributeNsSafe($rdfans, 'href'),
				{'element'=>$current_element,'xml_base'=>$hrefsrc_base}
				);
		}
		
		# Note that final value of the [current object resource] will either
		# be null (from initialization), a full URI or a bnode. 		
	}
	
	# If in any of the previous steps a [new subject] was set to a non-null
	# value, it is now used to provide a subject for type values
	if ($new_subject
	&& (  $current_element->hasAttributeNsSafe($rdfans, 'instanceof')
		|| $current_element->hasAttributeNsSafe($rdfans, 'typeof')))
	{

		if ($current_element->hasAttributeNsSafe($rdfans, 'instanceof')
		&&  $current_element->hasAttributeNsSafe($rdfans, 'typeof'))
		{
			$self->_log_error(
				ERR_WARNING,
				ERR_CODE_INSTANCEOF_OVERRULED,
				"Deprecated \@instanceof found; ignored because \@typeof also present.",
				element => $current_element,
				);
		}
		elsif ($current_element->hasAttributeNsSafe($rdfans, 'instanceof'))
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
	
		my @instanceof = $self->_split_tokens(  $current_element->getAttributeNsSafe($rdfans, 'typeof')
			|| $current_element->getAttributeNsSafe($rdfans, 'instanceof') );
		
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

			$self->_insert_triple_resource($current_element, $new_subject, RDF_TYPE, $rdftype, $graph);
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
	
	my @prop = $self->_split_tokens( $current_element->getAttributeNsSafe($rdfans, 'property') );

	my $has_datatype = 0;
	my $datatype = undef;
	if ($current_element->hasAttributeNsSafe($rdfans, 'datatype'))
	{
		$has_datatype = 1;
		$datatype = $self->_expand_curie(
			$current_element->getAttributeNsSafe($rdfans, 'datatype'),
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
		if ($current_element->hasAttributeNsSafe($rdfans, 'content'))
		{
			@current_object_literal = ($current_element->getAttributeNsSafe($rdfans, 'content'),
				($has_datatype ? $datatype : undef),
				$current_language);
		}
		
		# OpenDocument 1.2 extension
		elsif (defined $self->{'options'}->{'bookmark_end'}
		&& defined $self->{'options'}->{'bookmark_name'}
		&& (
			'{}'.$self->{'options'}->{'bookmark_start'} eq sprintf('{%s}%s',$current_element->namespaceURI,$current_element->localname)
			|| $self->{'options'}->{'bookmark_start'} eq sprintf('{%s}%s',$current_element->namespaceURI,$current_element->localname)
		))
		{
			@current_object_literal = ($self->_element_to_bookmarked_string($current_element),
				($has_datatype ? $datatype: undef),
				$current_language);
		}
		
		# Additionally, if there is a value for [current language] then
		# the value of the [plain literal] should include this language
		# information, as described in [RDF-CONCEPTS]. The actual literal
		# is either the value of @content (if present) or a string created
		# by concatenating the text content of each of the descendant
		# elements of the [current element] in document order. 
		
		# or all children of the [current element] are text nodes;
		# or there are no child nodes;
		# or the body of the [ current element ] does have non-text
		#    child nodes but @datatype is present, with an empty value. 
		elsif ((!$current_element->getElementsByTagName('*')) 
		||     ($has_datatype and $datatype eq ''))
		{
			@current_object_literal = ($self->_element_to_string($current_element),
				($has_datatype ? $datatype: undef),
				$current_language);
		}

		# as an [XML literal] if: explicitly rdf:XMLLiteral.
		elsif ($datatype eq RDF_XMLLIT)
		{
			@current_object_literal = ($self->_element_to_xml($current_element, $current_language),
				RDF_XMLLIT,
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
		elsif ($has_datatype)
		{
			if ($current_element->hasAttributeNsSafe($rdfans, 'content'))
			{
				@current_object_literal = ($current_element->getAttributeNsSafe($rdfans, 'content'),
					$datatype,
					$current_language);
			}
			else
			{
				@current_object_literal = ($self->_element_to_string($current_element),
					$datatype,
					$current_language);
			}
		}

		# In RDFa 1.0 by default generate an XML Literal;
		# in RDFa 1.1 by default generate a plain literal.
		elsif (!$has_datatype and $current_element->getElementsByTagName('*'))
		{
			if ($self->{'options'}->{'xmllit_default'})
			{
				@current_object_literal = ($self->_element_to_xml($current_element, $current_language),
					RDF_XMLLIT,
					$current_language);
			}
			else
			{
				@current_object_literal = ($self->_element_to_string($current_element),
					undef,
					$current_language);
			}
			$recurse = 0;
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

B<Advanced usage only.>

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
			if lc ($self->{'sub'}->{'pretriple_resource'}||'') eq 'print';
		$self->{'sub'}->{'pretriple_literal'} = \&_print1
			if lc ($self->{'sub'}->{'pretriple_literal'}||'') eq 'print';
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

	printf("%s %s %s%s%s .\n",
		($subject =~ /^_:/ ? $subject : "<$subject>"),
		"<$pred>",
		"\"$object\"",
		(length $dt ? "^^<$dt>" : ''),
		((length $lang && !length $dt) ? "\@$lang" : '')
		);
	
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
	# $object = encode_utf8($object);

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

sub _element_to_bookmarked_string
{
	my ($self, $bookmark) = @_;

	my @name_attribute;
	if ($self->{'options'}->{'bookmark_name'} =~ /^\{(.*)\}(.+)$/)
	{
		@name_attribute = $1 ? ($1, $2) : (undef, $2);
	}
	else
	{
		@name_attribute = (undef, $self->{'options'}->{'bookmark_name'});
	}
	
	my ($endtag_namespace, $endtag_localname);
	if ($self->{'options'}->{'bookmark_end'} =~ /^\{(.*)\}(.+)$/)
	{
		($endtag_namespace, $endtag_localname) = $1 ? ($1, $2) : (undef, $2);
	}
	else
	{
		($endtag_namespace, $endtag_localname) = (undef, $self->{'options'}->{'bookmark_end'});
	}

	my $string = '';
	my $current = $bookmark;
	while ($current)
	{
		$current = $self->_find_next_node($current);
		
		if (defined $current
		&& $current->nodeType == XML_TEXT_NODE)
		{
			$string .= $current->getData;
		}
		if (defined $current
		&& $current->nodeType == XML_ELEMENT_NODE
		&& $current->localname eq $endtag_localname
		&& $current->namespaceURI eq $endtag_namespace
		&& $current->getAttributeNsSafe(@name_attribute) eq $bookmark->getAttributeNsSafe(@name_attribute))
		{
			$current = undef;
		}
	}
	
	return $string;
}

sub _find_next_node
{
	my ($self, $node) = @_;
	
	if ($node->nodeType == XML_ELEMENT_NODE)
	{
		my @kids = $node->childNodes;
		return $kids[0] if @kids;
	}
	
	my $ancestor = $node;
	while ($ancestor)
	{
		return $ancestor->nextSibling if $ancestor->nextSibling;
		$ancestor = $ancestor->parentNode;
	}
	
	return undef;
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
	my $ident   = shift || undef;
	
	if (defined $element
	and $self->{'saved_bnodes'}->{ $element->nodePath })
	{
		return $self->{'saved_bnodes'}->{ $element->nodePath };
	}

	elsif (defined $ident
	and $self->{'saved_bnodes'}->{ $ident })
	{
		return $self->{'saved_bnodes'}->{ $ident };
	}

	return sprintf('http://thing-described-by.org/?%s#%s',
		$self->uri,
		$self->{element}->getAttribute('id'))
		if ($self->{options}->{tdb_service} && $element && length $element->getAttribute('id'));

	unless (defined $self->{bnode_prefix})
	{
		$self->{bnode_prefix} = Data::UUID->new->create_str;
		$self->{bnode_prefix} =~ s/-//g;
	}

	my $rv = sprintf('_:rdfa%snode%04d', $self->{bnode_prefix}, $self->{bnodes}++);
	
	if ($save_me and defined $element)
	{
		$self->{'saved_bnodes'}->{ $element->nodePath } = $rv;
	}

	if (defined $ident)
	{
		$self->{'saved_bnodes'}->{ $ident } = $rv;
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
	
	if (defined $self->{'sub'}->{'ontoken'})
	{
		return $self->{'sub'}->{'ontoken'}($self, $args{element}, $token, $r);
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
			{ $bnode = $self->bnode(undef, undef, '_:'); }
		elsif ($token =~ /^_:(.+)$/i || $token =~ /^\[_:(.+)\]$/i)
			{ $bnode = $self->bnode(undef, undef, '_:'.$1); }
		
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
	and ($is_safe || $args{'attribute'} =~ /^(rel|rev|property|typeof|datatype|role)$/i || $args{'allow_unsafe_term'}))
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
	and ($is_safe || $args{'attribute'} =~ /^(rel|rev|property|typeof|datatype|role)$/i || $args{'allow_unsafe_curie'}))
	{
		$token =~ /^($XML::RegExp::NCName)?:(\S+)$/;
		my $prefix = (defined $1 && length $1) ? $1 : '-';
		my $suffix = $2;
		
		if (defined $args{'prefixes'}->{'-'} && $prefix eq '-')
			{ return $args{'prefixes'}->{'-'} . $suffix; }
		elsif (defined $args{'prefixes'}->{'sensitive'}->{$prefix})
			{ return $args{'prefixes'}->{'sensitive'}->{$prefix} . $suffix; }
		elsif (defined $args{'prefixes'}->{'insensitive'}->{lc $prefix})
			{ return $args{'prefixes'}->{'insensitive'}->{lc $prefix} . $suffix; }

		if ($is_safe)
		{
			$prefix = ($prefix eq '-') ? '' : $prefix;
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
	and ($is_safe || $args{'attribute'} =~ /^(rel|rev|property|typeof|datatype|role)$/i || $args{'allow_unsafe_curie'}))
	{
		my $prefix = $token;
		my $suffix = '';
		
		if (defined $args{'prefixes'}->{'sensitive'}->{$prefix})
			{ return $args{'prefixes'}->{'sensitive'}->{$prefix} . $suffix; }
		elsif (defined $args{'prefixes'}->{'insensitive'}->{lc $prefix})
			{ return $args{'prefixes'}->{'insensitive'}->{lc $prefix} . $suffix; }
	}

	# Absolute URIs
	if ($token =~ /^[A-Z][A-Z0-9\.\+-]*:/i and !$is_safe
	and ($self->{'options'}->{'full_uris'} || $args{'attribute'} =~ /^(about|resource|graph)$/i))
	{
		return $token;
	}

	# CURIEs - default vocab
	if ($token =~ /^($XML::RegExp::NCName)$/
	and ($is_safe || $args{'attribute'} =~ /^(rel|rev|property|typeof|datatype|role)$/i || $args{'allow_unsafe_default_vocab'}))
	{
		my $suffix = $token;
		
		if (defined $args{'prefixes'}->{'*'})
			{ return $args{'prefixes'}->{'*'} . $suffix; }
	
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

# naughty hack
package XML::LibXML::Element;
sub getAttributeNsSafe
{
	my ($element, $nsuri, $attribute) = @_;
	return defined $nsuri ? $element->getAttributeNS($nsuri, $attribute) : $element->getAttribute($attribute);
}
sub hasAttributeNsSafe
{
	my ($element, $nsuri, $attribute) = @_;
	return defined $nsuri ? $element->hasAttributeNS($nsuri, $attribute) : $element->hasAttribute($attribute);
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

This is called when a CURIE or term has been expanded. The parameters are:

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

Most features are configurable using L<RDF::RDFa::Parser::Config>.

=head2 RDFa Versions

RDF::RDFa::Parser supports RDFa versions 1.0 and 1.1.

1.1 is currently a moving target; support is experimental.

1.1 is the default, but this can be configured using RDF::RDFa::Parser::Config.

=head2 Host Languages

RDF::RDFa::Parser supports various different RDFa host languages:

=over 4

=item * B<XHTML>

As per the XHTML+RDFa 1.0 and XHTML+RDFa 1.1 specifications.

=item * B<HTML 4>

Uses an HTML5 (sic) parser; uses @lang instead of @xml:lang; keeps prefixes
and terms case-insensitive; recognises the @rel relations defined in the HTML
4 specification. Otherwise the same as XHTML.

=item * B<HTML5>

Uses an HTML5 parser; uses @lang as well as @xml:lang; keeps prefixes
and terms case-insensitive; recognises the @rel relations defined in the HTML5
draft specification. Otherwise the same as XHTML.

=item * B<XML>

This is implemented as per the RDFa Core 1.1 specification. There is also
support for "RDFa Core 1.0", for which no specification exists, but has been
reverse-engineered by applying the differences between XHTML+RDFa 1.1 and
RDFa Core 1.1 to the XHTML+RDFa 1.0 specification.

Embedded chunks of RDF/XML within XML are supported.

=item * B<SVG>

For now, a synonym for XML.

=item * B<Atom>

The E<lt>feedE<gt> and E<lt>entryE<gt> elements are treated specially, setting
a new subject; IANA-registered rel keywords are recognised.

By passing C<< atom_parser=>1 >> as a Config option, you can also handle
Atom's native semantics. (Uses XML::Atom::OWL. If this module is not installed,
this option is silently ignored.)

Otherwise, the same as XML.

=item * B<DataRSS>

Support for the E<lt>?profile?E<gt> processing instruction; defines some default
prefixes. Otherwise, the same as Atom.

=item * B<OpenDocument XML>

That is, XML content formatted along the lines of 'content.xml' in OpenDocument
files.

Supports OpenDocument bookmarked ranges used as typed or plain object literals
(though not XML literals); expects RDFa attributes in the XHTML namespace
instead of in no namespace. Otherwise, the same as XML.

=item * B<OpenDocument>

That is, a ZIP file containing OpenDocument XML files. RDF::RDFa::Parser
will do all the unzipping and combining for you, so you don't have to.
The unregistered "jar:" URI scheme is used to refer to files within the ZIP.

=back

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

=item 1. Parse the RDF/XML.

The parser will parse the RDF/XML properly. If named graphs are
enabled, any triples will be added to a separate graph. This is
the behaviour that SVG Tiny 1.2 seems to suggest is the correct
thing to do.

=item 2. Skip the chunk.

This will skip over the RDF element entirely, and thus save you a
bit of time.

=back

You can decide which path to take by setting the 'embedded_rdfxml'
Config option. For HTML and XHTML, you probably want
to set embedded_rdfxml to '0' (the default) or '2' (a little faster).
For other XML markup languages (e.g. SVG or Atom), then you probably want to
set it to '1'.

(There's also an option '3' which controls how embedded RDF/XML interacts
with named graphs, but this is only really intended for internal use, parsing
OpenDocument.)

=head2 Named Graphs

The parser has support for named graphs within a single RDFa
document. To switch this on, use the 'graph' Config option.

See also L<http://buzzword.org.uk/2009/rdfa4/spec>.

The name of the attribute which indicates graph URIs is by
default 'graph', but can be changed using the 'graph_attr'
Config option. This option accepts Clark Notation to specify a
namespaced attribute. By default, the attribute value is
interpreted as a fragment identifier (like the 'id' attribute),
but if you set the 'graph_type' Config option to 'about',
it will be treated as a URI or safe CURIE (like the 'about'
attribute).

The 'graph_default' Config option allows you to set the default
graph URI/bnode identifier.

Once you're using named graphs, the C<graphs> method becomes
useful: it returns a hashref of { graph_uri => trine_model } pairs.
The optional parameter to the C<graph> method also becomes useful.

OpenDocument (ZIP) host language support makes internal use
of named graphs, so if you're parsing OpenDocument, tinker with
the Config options at your own risk!

=head2 Auto Config

RDF::RDFa::Parser has a lot of different Config options to play with. Sometimes it
might be useful to allow the page being parsed to control some of these options.
If you switch on the 'auto_config' Config option, pages can do this.

A page can set options using a specially crafted E<lt>metaE<gt> tag:

  <meta name="http://search.cpan.org/dist/RDF-RDFa-Parser/#auto_config"
     content="xhtml_lang=1&amp;xml_lang=0" />

Note that the C<content> attribute is an application/x-www-form-urlencoded
string (which must then be HTML-escaped of course). Semicolons may be used
instead of ampersands, as these tend to look nicer:

  <meta name="http://search.cpan.org/dist/RDF-RDFa-Parser/#auto_config"
     content="xhtml_lang=1;xml_lang=0" />

It's possible to use auto config outside XHTML (e.g. in Atom or
SVG) using namespaces:

  <xhtml:meta xmlns:xhtml="http://www.w3.org/1999/xhtml"
     name="http://search.cpan.org/dist/RDF-RDFa-Parser/#auto_config"
     content="xhtml_lang=0;xml_base=2;atom_elements=1" />

Any Config option may be given using auto config, except 'use_rtnlx', 'dom_parser',
and of course 'auto_config' itself. 

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
