#!/usr/bin/perl

=head1 NAME

RDF::RDFa::Parser - RDFa parser using XML::LibXML.

=head1 SYNOPSIS

 use RDF::RDFa::Parser;
 
 $parser = RDF::RDFa::Parser->new($xhtml, $baseuri);
 $parser->consume;
 $graph = $parser->graph;

=cut

package RDF::RDFa::Parser;
use Carp;
use RDF::Trine;
use URI::URL;
use XML::LibXML qw(:all);
use strict;

=head1 VERSION

0.20

Note: version 0.20 introduced major incompatibilities with 0.0x and 0.1x.

=cut

our $VERSION = '0.20';

=head1 PUBLIC METHODS

=over

=item $p = RDF::RDFa::Parser->new($xhtml, $baseuri, \%options, $storage)

This method creates a new C<RDF::RDFa::Parser> object and returns it.

The $xhtml variable may contain an XHTML/XML string, or a
XML::LibXML::Document. If a string, the document is parsed using
C<XML::LibXML>, which will throw an exception if it is not
well-formed. C<RDF::RDFa::Parser> does not catch the exception.

The base URI is used to resolve relative URIs found in the document.

Options (mostly booleans) [default in brackets]:

  * full_uris       - Support full URIs in CURIE-only attributes. [0]
  * strict_reserved_words  - Be strict about reserved words. [1]
  * xfn_reserved_words     - Allow XFN 1.1 reserved words. [0]
  * embedded_rdfxml - Find plain RDF/XML chunks within document. [0]
                      0=no, 1=handle, 2=skip.
  * xml_base        - Support for 'xml:base' attribute. [0]
                      0=only RDF/XML; 1=except @href/@src; 2=always.
  * xhtml_elements  - Process <head> and <body> specially. [1]
  * xhtml_lang      - Support @lang rather than just @xml:lang. [0]
  * xhtml_base      - Process <base> element. [1]
                      0=no, 1=yes, 2=use it for RDF/XML too
  * prefix_attr     - Support @prefix rather than just @xmlns:foo. [0]
  * tdb_service     - Use thing-described-by.org to name some bnodes. [0]

The default options attempt to stick to the XHTML+RDFa spec as rigidly
as possible.

$storage is an RDF::Trine::Storage object. If undef, then a new
temporary store is created.

=cut

sub new
{
	my $class   = shift;
	my $xhtml   = shift;
	my $baseuri = shift;
	my $options = shift;
	my $store   = shift;
	my $DOMTree;

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
			'sub'     => [],
			'options' => {
				'named_graphs'          => 0,
				'xml_base'              => 0,
				'full_uris'             => 0,
				'strict_reserved_words' => 1,
				'xfn_reserved_words'    => 0,
				'xhtml_elements'        => 1,
				'xhtml_lang'            => 0,
				'prefix_attr'           => 0,
				'tdb_service'           => 0,
				'xhtml_base'            => 1,
				'embedded_rdfxml'       => 0,
			},
			'Graphs'  => {},
		};
	bless $this, $class;
	
	foreach my $o (keys %$options)
	{
		$this->{'options'}->{$o} = $options->{$o};
	}

	# HTML <base> element.
	if ($this->{'options'}->{'xhtml_base'})
	{
		my @bases = $this->{DOM}->getElementsByTagName('base');
		my $base;
		foreach my $b (@bases)
		{
			$base = $b->getAttribute('href')
				if (length $b->getAttribute('href'));
		}
		$this->{baseuri} = $this->uri($base)
			if (defined $base && length $base);
	}
	
	return $this;
}

=item $p->xhtml

Returns the XHTML source of the document being parsed.

=cut

sub xhtml
{
	my $this = shift;
	return $this->{xhtml};
}

=item $p->uri

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
	return $url->abs->as_string;
}

=item $p->dom

Returns the parsed C<XML::LibXML::Document>.

=cut

sub dom
{
	my $this = shift;
	return $this->{DOM};
}

=item $p->set_callbacks(\&func1, \&func2)

Set callbacks for handling RDF triples extracted from RDFa document. The
first function is called when a triple is generated taking the form of
(I<resource>, I<resource>, I<resource>). The second function is called when a
triple is generated taking the form of (I<resource>, I<resource>, I<literal>).

The parameters passed to the first callback function are:

=over 4

=item * A reference to the C<RDF::RDFa::Parser> object

=item * A reference to the C<XML::LibXML element> being parsed

=item * Subject URI or bnode

=item * Predicate URI

=item * Object URI or bnode

=item * Graph URI or bnode (if named graphs feature is enabled)

=back

The parameters passed to the second callback function are:

=over 4

=item * A reference to the C<RDF::RDFa::Parser> object

=item * A reference to the C<XML::LibXML element> being parsed

=item * Subject URI or bnode

=item * Predicate URI

=item * Object literal

=item * Datatype URI (possibly undef or '')

=item * Language (possibly undef or '')

=item * Graph URI or bnode (if named graphs feature is enabled)

=back

In place of either or both functions you can use the string C<'print'> which
sets the callback to a built-in function which prints the triples to STDOUT
as Turtle. Either or both can be set to undef, in which case, no callback
is called when a triple is found.

Beware that for literal callbacks, sometimes both a datatype *and* a language
will be passed. (This goes beyond the normal RDF data model.)

C<set_callbacks> must be used I<before> C<consume>.

IMPORTANT - CHANGED IN VERSION 0.20 - callback functions should return true
if they wish to prevent the triple from being added to the parser's built-in
model; false otherwise.

=cut

sub set_callbacks
# Set callback functions for handling RDF triples.
{
	my $this = shift;

	for (my $n=0 ; $n<2 ; $n++)
	{
		if (lc($_[$n]) eq 'print')
			{ $this->{'sub'}->[$n] = ($n==0 ? \&_print0 : \&_print1); }
		elsif ('CODE' eq ref $_[$n])
			{ $this->{'sub'}->[$n] = $_[$n]; }
		else
			{ $this->{'sub'}->[$n] = undef; }
	}
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

=item $p->named_graphs($xmlns, $attribute, $attributeType)

C<RDF::RDFa::Parser> allows for one RDFa document to generate multiple graphs.
A graph is created by enclosing it in an element with an attribute with XML
namespace $xmlns and local name $attribute. 

Each graph is given a URI - if $attributeType is the string 'id', then the URI
is generated by treating the attribute like an 'id' attribute - i.e. the URI is
the document's base URI, followed by a hash, followed by the attribute value.
If $attributeType is the string 'about', then the URI is generated by treating
the attribute like an 'about' attribute - i.e. it is treated as an absolute or
relative URI, with safe CURIEs being allowed too. If the $attributeType is
omitted, then the default behaviour is 'about'.

Calling this method with no parameters will disable the named graph feature.
Named graphs are disabled by default.

C<named_graphs> must be used I<before> C<consume>.

=cut

sub named_graphs
{
	my $this  = shift;
	my $xmlns = shift;
	my $attr  = shift;
	my $type  = shift || 'about';
	
	if (defined $xmlns)
	{
		die "Must specify XML namespace and attribute.\n" unless defined $attr;
		
		$this->{'options'}->{'named_graphs'} = {
			'xmlns'         => $xmlns,
			'attr'          => $attr,
			'type'          => lc($type),
			'default'       => '_:RDFaDefaultGraph',
			'default_trine' => RDF::Trine::Node::Blank->new('RDFaDefaultGraph'),
		};
	}

	else
	{
		$this->{'options'}->{'named_graphs'} = 0;
	}

	return $this;
}

=item $p->thing_described_by(1)

C<RDF::RDFa::Parser> has a feature that allows it to use thing-described-by.org
to create URIs for some blank nodes. It is disabled by default. This function
can be used to turn it on (1) or off (0). May be called without a parameter,
which just returns the current status.

C<thing_described_by> must be used I<before> C<consume>.

THIS FUNCTION IS DEPRECATED. PASS AN OPTION TO THE CONSTRUCTOR INSTEAD.

=cut

sub thing_described_by
{
	my $this = shift;
	my $set  = shift;
	
	my $rv   = $this->{options}->{tdb_service};
	
	$this->{options}->{tdb_service} = $set
		if (defined $set);
		
	return $rv;
}

=item $p->consume

The document is parsed for RDFa. Nothing of interest is returned by this
function, but the triples extracted from the document are passed to the
callbacks as each one is found.

=cut

sub consume
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
	my $uri_mappings       = shift || { 'xml' => XML_XML_NS, 'xmlns' => XML_XMLNS_NS , '' => 'http://www.w3.org/1999/xhtml/vocab#' };
	my $incomplete_triples = shift || ();
	my $language           = shift || undef;
	my $graph              = shift || ($this->{'options'}->{'named_graphs'} ? $this->{'options'}->{'named_graphs'}->{'default'} : undef);
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
	if ($current_element->hasAttributeNS(XML_XML_NS, 'lang'))
	{
		$current_language = $current_element->getAttributeNS(XML_XML_NS, 'lang')
			if valid_lang($current_element->getAttributeNS(XML_XML_NS, 'lang'));
	}

	# EXTENSION
	# xml:base - important for RDF/XML extension
	if ($current_element->hasAttributeNS(XML_XML_NS, 'base'))
	{
		$xml_base = $current_element->getAttributeNS(XML_XML_NS, 'base');
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
						($this->{'options'}->{'named_graphs'} ? $g : undef));
				}
				else
				{
					$o[0] = $st->object->is_blank ?
						($g.'_'.$st->object->blank_identifier) :
						$st->object->uri_value ;
					$this->rdf_triple(undef, $s, $p, @o,
						($this->{'options'}->{'named_graphs'} ? $g : undef));
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
			my $pfx = $1;
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
				$local_uri_mappings->{$pfx} = $uri;
			}
		}
	}
	
	# EXTENSION
	# The rdfa.info wiki includes an additional proposed syntax for defining
	# prefixes. This uses a single @prefix attribute, making it more DTD
	# friendly. Let's support that too.
	if ($this->{'options'}->{'prefix_attr'}
	&& $current_element->getAttribute('prefix'))
	{
		my $pfx_attr = $current_element->getAttribute('prefix');
		while ($pfx_attr =~ /^\s*(\S*)[\s\r\n]*=[\s\r\n]*(\S*)[\s\r\n]+/gs)
		{
			my $pfx = $1;
			my $uri = $2;
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
				$local_uri_mappings->{$pfx} = $uri;
			}
		}
	}
	
	# EXTENSION
	# KjetilK's named graphs.
	if ($this->{'options'}->{'named_graphs'})
	{
		if ($this->{'options'}->{'named_graphs'}->{'type'} eq 'id'
		&&  defined $current_element->getAttributeNS(
				$this->{'options'}->{'named_graphs'}->{'xmlns'},
				$this->{'options'}->{'named_graphs'}->{'attr'}))
		{
			$graph = $this->uri('#' . $current_element->getAttributeNS(
					$this->{'options'}->{'named_graphs'}->{'xmlns'},
					$this->{'options'}->{'named_graphs'}->{'attr'}));
		}
		elsif ($this->{'options'}->{'named_graphs'}->{'type'} eq 'about'
		&&  defined $current_element->getAttributeNS(
				$this->{'options'}->{'named_graphs'}->{'xmlns'},
				$this->{'options'}->{'named_graphs'}->{'attr'}))
		{
			$graph = uriOrSafeCurie($this, 
				$current_element->getAttributeNS(
					$this->{'options'}->{'named_graphs'}->{'xmlns'},
					$this->{'options'}->{'named_graphs'}->{'attr'}),
				$current_element, $local_uri_mappings, $xml_base);
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
	my @REL;
	foreach my $r (@rel)
	{
		my $R = reservedWordOrCurie($this, $r, $current_element, $local_uri_mappings, $xml_base);
		push @REL, $R if (length $R);
	}
	my $rev = $current_element->getAttribute('rev');
	$rev =~ s/(^\s+|\s+$)//g;
	$rev =~ s/\s+/ /g;
	my @rev = split / /, $rev;
	my @REV;
	foreach my $r (@rev)
	{
		my $R = reservedWordOrCurie($this, $r, $current_element, $local_uri_mappings, $xml_base);
		push @REV, $R if (length $R);
	}
	if ((!defined $current_element->getAttribute('rel'))
	&&  (!defined $current_element->getAttribute('rev')))
	{
		# [new subject] is set to the URI obtained from the first match
		# from the following rules:
		
		# by using the URI from @about, if present, obtained according to
		# the section on CURIE and URI Processing ; 
		if (defined $current_element->getAttributeNode('about'))
			{ $new_subject = uriOrSafeCurie($this, $current_element->getAttribute('about'), $current_element, $local_uri_mappings, $xml_base); }
			
		# otherwise, by using the URI from @src, if present, obtained
		# according to the section on CURIE and URI Processing.
		elsif (defined $current_element->getAttributeNode('src'))
			{ $new_subject = $this->uri($current_element->getAttribute('src'), {'element'=>$current_element,'xml_base'=>$hrefsrc_base}); }
			
		# otherwise , by using the URI from @resource, if present, obtained
		# according to the section on CURIE and URI Processing ; 
		elsif (defined $current_element->getAttributeNode('resource'))
			{ $new_subject = uriOrSafeCurie($this, $current_element->getAttribute('resource'), $current_element, $local_uri_mappings, $xml_base); }
			
		# otherwise , by using the URI from @href, if present, obtained
		# according to the section on CURIE and URI Processing.
		elsif (defined $current_element->getAttributeNode('href'))
			{ $new_subject = $this->uri($current_element->getAttribute('href'), {'element'=>$current_element,'xml_base'=>$hrefsrc_base}); }
			
		# If no URI is provided by a resource attribute, then the first
		# match from the following rules will apply: 
		else
		{
			# if the element is the head or body element then act as if
			# there is an empty @about present, and process it according to
			# the rule for @about, above; 
			if ($this->{'options'}->{'xhtml_elements'}
			&& ($current_element->tagName eq 'head' || $current_element->tagName eq 'body'))
			{
				$new_subject = $this->uri;
			}
			
			# if @instanceof is present, obtained according to the section
			# on CURIE and URI Processing , then [new subject] is set to be
			# a newly created [bnode]; 
			elsif (defined $current_element->getAttributeNode('typeof')
				||  defined $current_element->getAttributeNode('instanceof'))
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
					unless (defined $current_element->getAttributeNode('property'));
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
		if (defined $current_element->getAttributeNode('about'))
			{ $new_subject = uriOrSafeCurie($this, $current_element->getAttribute('about'), $current_element, $local_uri_mappings, $xml_base); }
			
		# otherwise, by using the URI from @src, if present, obtained
		# according to the section on CURIE and URI Processing.
		elsif (defined $current_element->getAttributeNode('src'))
			{ $new_subject = $this->uri($current_element->getAttribute('src'), {'element'=>$current_element,'xml_base'=>$hrefsrc_base}); }

		# If no URI is provided then the first match from the following rules
		# will apply: 
		else
		{
			# if the element is the head or body element then act as if
			# there is an empty @about present, and process it according
			# to the rule for @about, above; 
			if ($this->{'options'}->{'xhtml_elements'}
			&& ($current_element->tagName eq 'head' || $current_element->tagName eq 'body'))
			{
				$new_subject = $this->uri;
			}
			
			# if @instanceof is present, obtained according to the section
			# on CURIE and URI Processing, then [new subject] is set to be a
			# newly created [bnode]; 
			elsif (defined $current_element->getAttributeNode('typeof')
				||  defined $current_element->getAttributeNode('instanceof'))
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
		if (defined $current_element->getAttributeNode('resource'))
			{ $current_object_resource = uriOrSafeCurie($this, $current_element->getAttribute('resource'), $current_element, $local_uri_mappings, $xml_base); }
			
		# otherwise, by using the URI from @href, if present, obtained according
		# to the section on CURIE and URI Processing.
		elsif (defined $current_element->getAttributeNode('href'))
			{ $current_object_resource = $this->uri($current_element->getAttribute('href'), {'element'=>$current_element,'xml_base'=>$hrefsrc_base}); }
		
		# Note that final value of the [current object resource] will either
		# be null (from initialization), a full URI or a bnode. 
	}
	
	# If in any of the previous steps a [new subject] was set to a non-null
	# value, it is now used to provide a subject for type values
	if ($new_subject
	&& defined ($current_element->getAttributeNode('instanceof')
		.$current_element->getAttributeNode('typeof')))
	{
		# One or more 'types' for the [ new subject ] can be set by using
		# @instanceof. If present, the attribute must contain one or more
		# URIs, obtained according to the section on URI and CURIE Processing...
	
		my $instanceof = $current_element->getAttribute('typeof');
		$instanceof = $current_element->getAttribute('instanceof')
			unless defined($instanceof);
		$instanceof =~ s/\s+/ /g;
		$instanceof =~ s/(^\s|\s$)//g;
		my @instanceof = split / /, $instanceof;
		
		foreach my $instanceof (@instanceof)
		{
			my $rdftype = curie($this, $instanceof, $current_element, $local_uri_mappings, $xml_base);
			next unless $rdftype;
		
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
	if (defined $current_element->getAttributeNode('datatype'))
		{ $datatype = curie($this, $current_element->getAttribute('datatype'), $current_element, $local_uri_mappings, $xml_base); }
		
	if (length $prop)
	{
	
		# Predicates for the [current object literal] can be set by using
		# @property. If present, one or more URIs are obtained according
		# to the section on CURIE and URI Processing and then the actual
		# literal value is obtained as follows:
		
		# as a [ plain literal ] if: 
		#
		# @content is present; 
		if (defined $current_element->getAttributeNode('content'))
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
			@current_object_literal = ($this->xmlify($current_element),
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
			if (defined $current_element->getAttribute('content'))
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

		my $p = curie($this, $property, $current_element, $local_uri_mappings, $xml_base);
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
				$flag = $this->consume(
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
				$flag = $this->consume(
					$kid,
					$base,
					$new_subject,
					(defined $current_object_resource ? $current_object_resource : ($new_subject ? $new_subject : $parent_subject)),
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

sub rdf_triple
# Function only used internally.
{
	my $this = shift;

	my $suppress_triple = 0;
	if ($this->{'sub'}->[0])
	{
		$suppress_triple = $this->{'sub'}->[0]($this, @_);
	}
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
	if ($this->{'sub'}->[1])
	{
		$suppress_triple = $this->{'sub'}->[1]($this, @_);
	}
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

	if (defined $datatype)
	{
		# if (0) because current release of RDF::Trine doesn't include this class.
		if (0 && $datatype eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral')
		{
			$object = $element->childNodes;
			$to = RDF::Trine::Node::Literal::XML->new($object);
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

	# If we are configured for it, and graph name can be found, add it.
	if (ref($this->{'options'}->{'named_graphs'}) && ($graph))
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

		my $statement = RDF::Trine::Statement::Quad->new($ts, $tp, $to, $tg);
		$this->{RESULTS}->add_statement($statement);
	
		#if ($graph ne $this->{'options'}->{'named_graphs'}->{'default'})
		#{
		#	my $graph_statement = RDF::Trine::Statement::Quad->new($ts, $tp, $to, 
		#		$this->{'options'}->{'named_graphs'}->{'default_trine'});
		#	$this->{RESULTS}->add_statement($graph_statement,
		#		$this->{'options'}->{'named_graphs'}->{'default_trine'});
		#}
	}
	else
	{
		# If no graph name, just add triples
		my $statement = RDF::Trine::Statement->new($ts, $tp, $to);
		$this->{RESULTS}->add_statement($statement);
	}
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
	my $rv;
	
	foreach my $kid ($dom->childNodes)
	{
		$rv .= $kid->toStringEC14N(1);
	}
	
	return $rv;
}

sub bnode
# Function only used internally.
{
	my $this    = shift;
	my $element = shift;
	
	return sprintf('http://thing-described-by.org/?%s#%s',
		$this->uri,
		$this->{element}->getAttribute('id'))
		if ($this->{options}->{tdb_service} && $element && length $element->getAttribute('id'));

	return sprintf('_:RDFaAutoNode%03d', $this->{bnodes}++);
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
	my $maps  = shift;
	my $xbase = shift;
	my $xpath = $dom->getAttribute('_xpath') if ($dom);
	my $safe  = 0;
	
	# Blank nodes
	if ($str eq '_:' || $str eq '[_:]')
	{
		return '_:RDFaX';
	}
	elsif ($str =~ /^_:RDFa(.*)$/i || $str =~ /^\[_:RDFa(.*)\]$/i)
	{
		return '_:RDFax'.$1;
	}
	elsif ($str =~ /^_:(.+)$/i || $str =~ /^\[_:(.+)\]$/i)
	{
		return '_:'.$1;
	}
	
	$str = 'xhv'.$str if ($str =~ /^\:/);
	
	# Cope with safe CURIEs.
	if ($str =~ /^\[(.*)\]$/)
	{
		$str  = $1;
		$safe = 1;
	}
	
	# If the string matches CURIE syntax, then resolve the CURIE.
	if ($str =~ /^([a-z_][^\s\:]*)?\:(.*)$/i)
	{
		my $pfx = $1 || '';
		my $sfx = $2;
		my $ns  = $maps->{$pfx};
		
		return $page->uri($ns.$sfx,{'element'=>$dom,'xml_base'=>$xbase})
			if ($ns);
			
		return 'http://invalid.invalid/ns#'.$sfx
			if ($safe);
	}
	
	# If it wasn't a safe CURIE, then fall back to a URI.
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
	my $maps  = shift;
	my $xbase = shift;
	my $xpath = $dom->getAttribute('_xpath') if ($dom);
	my $safe  = 0;
	
	# Blank nodes
	if ($str eq '_:' || $str eq '[_:]')
	{
		return '_:RDFaX';
	}
	elsif ($str =~ /^_:RDFa(.*)$/i || $str =~ /^\[_:RDFa(.*)\]$/i)
	{
		return '_:RDFax'.$1;
	}
	elsif ($str =~ /^_:(.+)$/i || $str =~ /^\[_:(.+)\]$/i)
	{
		return '_:'.$1;
	}

	$str = 'xhv'.$str if ($str =~ /^\:/);
	
	# Cope with safe CURIEs.
	if ($str =~ /^\[(.*)\]$/)
	{
		$str  = $1;
		$safe = 1;
	}
	
	# Only safe CURIEs allowed
	if ($safe)
	{
		# If the string matches CURIE syntax, then resolve the CURIE.
		if ($str =~ /^([a-z_][^\s\:]*)?\:(.*)$/i)
		{
			my $pfx = $1 || '';
			my $sfx = $2;
			my $ns  = $maps->{$pfx};
			
			return $page->uri($ns.$sfx,{'element'=>$dom,'xml_base'=>$xbase})
				if ($ns);
			
			return 'http://invalid.invalid/ns#'.$sfx
				if ($safe);
		}
	}
	
	# If it wasn't a safe CURIE, then fall back to a URI.
	else
	{
		return $page->uri($str, {'element'=>$dom,'xml_base'=>$xbase});
	}
	
	return undef;
}

sub reservedWordOrCurie
# Function only used internally.
{
	my $page  = shift;
	my $str   = shift;
	my $dom   = shift;
	my $maps  = shift;
	my $xbase = shift;
	my $xpath = $dom->getAttribute('_xpath') if ($dom);
	my $safe  = 0;

	my $xhv = 'http://www.w3.org/1999/xhtml/vocab#';
	
	# Blank nodes
	if ($str eq '_:' || $str eq '[_:]')
	{
		return '_:RDFaX';
	}
	elsif ($str =~ /^_:RDFa(.*)$/i || $str =~ /^\[_:RDFa(.*)\]$/i)
	{
		return '_:RDFax'.$1;
	}
	elsif ($str =~ /^_:(.+)$/i || $str =~ /^\[_:(.+)\]$/i)
	{
		return '_:'.$1;
	}
	
	if ($page->{'options'}->{'strict_reserved_words'})
	{
		if ($str =~ /^\:?( alternate | appendix | bookmark | cite |
			chapter | contents | copyright | first | glossary | help |
			icon | index | last | license | meta | next | p3pv1 |
			prev | role | section | stylesheet | subsection | start |
			top | up )$/ix)
		{
			return $xhv.lc($1);
		}
	}
	else
	{
		# From HTML 4.01 and (in all-caps) HTML 3.2 specs 
		if ($str =~ /^\:?( Alternate | Stylesheet | Start | Next | Prev |
			Contents | Index | Glossary | Copyright | Chapter | Section |
			Subsection | Appendix | Help | Bookmark | PREVIOUS | SEARCH |
			MADE | TOP )$/ix)
		{
			return $xhv.lc($1);
		}
		# And the HTML 5 spec.
		# 2008-07-20 : removed 'contact' as it's gone from HTML 5 draft.
		elsif ($str =~ /^\:?( alternate | archives | author | bookmark |
			external | feed | first | help | icon | index | last | license | next |
			nofollow | noreferrer | pingback | prefetch | prev | search |
			stylesheet | sidebar | tag | up )$/ix)
		{
			return $xhv.lc($1);
		}
		# Selected values from XHTML 2
		elsif ($str =~ /^\:?( cite | meta | p3pv1 | role )$/ix)
		{
			return $xhv.lc($1);
		}
		# GRDDL
		elsif (lc($str) eq 'transformation')
			{ return 'http://www.w3.org/2003/g/data-view#transformation'; }
		elsif (lc($str) eq 'profiletransformation')
			{ return 'http://www.w3.org/2003/g/data-view#profileTransformation'; }
		elsif (lc($str) eq 'namespacetransformation')
			{ return 'http://www.w3.org/2003/g/data-view#namespaceTransformation'; }
		# IANA Link Type Registry
		elsif ($str =~ /^\:?( alternate | appendix | bookmark | chapter |
			contents | copyright | current | describedby | edit | edit-media |
			enclosure | first | glossary | help | index | last | license |
			next | next-archive | payment | prev | previous | prev-archive |
			related | replies | section | self | service | start | stylesheet |
			subsection | up | via )$/ix)
		{
			return "http://www.iana.org/assignments/relation/".lc($1);
		}
		# XFN 1.1
		elsif ($page->{'options'}->{'xfn_reserved_words'}
		&& $str =~ /^\:?( contact | acquaintance | friend | met |
				co-worker | colleague | co-resident | neighbor | child |
				parent | sibling | spouse | kin | muse | crush | date |
				sweetheart | me )$/ix)
		{
			return "http://vocab.sindice.com/xfn#".lc($1)."-hyperlink";
		}
	}
		
	# Cope with safe CURIEs.
	if ($str =~ /^\[(.*)\]$/)
	{
		$str  = $1;
		$safe = 1;
	}
	
	# If the string matches CURIE syntax, then resolve the CURIE.
	if ($str =~ /^([a-z_][^\s\:]*)?\:(.*)$/i)
	{
		my $pfx = $1 || '';
		my $sfx = $2;
		my $ns  = $maps->{$pfx};
		
		return $page->uri($ns.$sfx, {'element'=>$dom,'xml_base'=>$xbase})
			if ($ns);
			
		return 'http://invalid.invalid/ns#'.$sfx
			if ($safe);
	}
	
	# If it wasn't a safe CURIE, then fall back to a URI.
	if (!$safe && $page->{'options'}->{'full_uris'})
	{
		return $page->uri($str, {'require-absolute'=>1,'element'=>$dom,'xml_base'=>$xbase});
	}
	
	return undef;
}

=item $p->graph( [ $graph_name ] ) 

Without a graph name, this method will return an RDF::Trine::Model
object with all statements of the full graph. As per the RDFa
specification, it will always return an unnamed graph containing all
the triples of the RDFa document. If the model contains multiple
graphs, all triples will be returned unless a graph name is specified.

It will also take an optional graph URI as argument, and return an
RDF::Trine::Model tied to a temporary storage with all triples in that
graph.

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

=item $p->graphs

Will return a hashref of all named graphs, where the graph name is a
key and the value is a RDF::Trine::Model tied to a temporary storage.

=back

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

=head1 CONSTANTS

=over 8

=item RDF::RDFa::Parser::OPTS_XHTML

Suggested options hashref for parsing XHTML.

=cut

sub OPTS_XHTML
{
	return {};
}

=item RDF::RDFa::Parser::OPTS_SVG

Suggested options hashref for parsing SVG.

=cut

sub OPTS_SVG
{
	return &OPTS_XML;
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

=back

=cut
			
1;

=head1 SEE ALSO

L<RDF::RDFa::Parser::Trine>, L<XML::LibXML>.

L<http://buzzword.org.uk/swignition/rdfa>

=head1 COPYRIGHT

 Copyright 2008, 2009 Toby Inkster

 This file is dual licensed under:
 The Artistic License
 GNU General Public License 3.0

 You may choose which of those two licences you are going to honour the
 terms of, but you cannot pick and choose the parts which you like of
 each. You must fulfil the licensing requirements of at least one of the
 two licenses.

 The Artistic License
 <http://www.perl.com/language/misc/Artistic.html>

 GNU General Public License 3.0
 <http://www.gnu.org/licenses/gpl-3.0.html>

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program. If not, see <http://www.gnu.org/licenses/>.

=cut
