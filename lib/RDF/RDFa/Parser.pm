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
use URI::URL;
use XML::LibXML;
use strict;

=head1 VERSION

0.11

=cut

our $VERSION = 0.11;

=head1 PUBLIC METHODS

=over

=item $p = RDF::RDFa::Parser->new($xhtml, $baseuri)

This method creates a new C<RDF::RDFa::Parser> object and returns it. The
XHTML document is parsed using C<XML::LibXML>, which will throw an exception
if it is not well-formed. C<RDF::RDFa::Parser> does not catch the exception.
The base URI is used to resolve relative URIs found in the document.

=cut

sub new
{
	my $class   = shift;
	my $xhtml   = shift;
	my $baseuri = shift;

	my $parser  = XML::LibXML->new;
	$parser->validation(0);
	$parser->recover(1);
	
	my $DOMTree = $parser->parse_string($xhtml);

	my $this = {
			'xhtml'   => $xhtml,
			'baseuri' => $baseuri,
			'DOM'     => $DOMTree,
			'RDF'     => {},
			'bnodes'  => 0,
			'tdb'     => 0,
			'sub'     => [],
			'named_graphs' => 0,
			'Graphs'  => {},
		};
	bless $this, $class;	

	# HTML <base> element.
	my @bases = $this->{DOM}->getElementsByTagName('base');
	my $base;
	foreach my $b (@bases)
	{
		$base = $b->getAttribute('href')
			if (length $b->getAttribute('href'));
	}
	$this->{baseuri} = $this->uri($base)
		if (defined $base && length $base);
		
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
	
	if ('XML::LibXML::Element' eq ref $opts)
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
	
	my $url = url $param, $this->{baseuri};
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

=item * Resource URI or bnode

=item * Graph URI or bnode (if named graphs feature is enabled)

=back

The parameters passed to the second callback function are:

=over 4

=item * A reference to the C<RDF::RDFa::Parser> object

=item * A reference to the C<XML::LibXML element> being parsed

=item * Subject URI or bnode

=item * Predicate URI

=item * Resource literal

=item * Datatype URI (possibly undef or '')

=item * Language (possibly undef or '')

=item * Graph URI or bnode (if named graphs feature is enabled)

=back

In place of either or both functions you can use the string C<'print'> which
sets the callback to a built-in function which prints the triples to STDOUT
as Turtle. Either or both can be set to undef, in which case, no callback
is called when a triple is found.

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
	
	printf("# Triple on element %s.\n", $element->nodePath);
	
	printf("%s %s %s .\n",
		($subject =~ /^_:/ ? $subject : "<$subject>"),
		"<$pred>",
		($object =~ /^_:/ ? $object : "<$object>"));
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
	
	# Clumsy, but probably works.
	$object =~ s/\\/\\\\/g;
	$object =~ s/\n/\\n/g;
	$object =~ s/\r/\\r/g;
	$object =~ s/\t/\\t/g;
	$object =~ s/\"/\\\"/g;
	
	printf("# Triple on element %s.\n", $element->nodePath);

	no warnings;
	printf("%s %s %s%s%s .\n",
		($subject =~ /^_:/ ? $subject : "<$subject>"),
		"<$pred>",
		"\"$object\"",
		(length $dt ? "^^<$dt>" : ''),
		((length $lang && !length $dt) ? "\@$lang" : '')
		);
	use warnings;
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
		
		$this->{'named_graphs'} = {
			'xmlns'  => $xmlns,
			'attr'   => $attr,
			'type'   => lc($type),
		};
	}

	else
	{
		$this->{'named_graphs'} = 0;
	}

	return $this;
}

=item $p->thing_described_by(1)

C<RDF::RDFa::Parser> has a feature that allows it to use thing-described-by.org
to create URIs for some blank nodes. It is disabled by default. This function
can be used to turn it on (1) or off (0). May be called without a parameter,
which just returns the current status.

=cut

sub thing_described_by
{
	my $this = shift;
	my $set  = shift;
	
	my $rv   = $this->{tdb};
	
	$this->{tdb} = $set
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
	my $uri_mappings       = shift || {};
	my $incomplete_triples = shift || ();
	my $language           = shift || undef;
	my $graph              = shift || ($this->{'named_graphs'} ? '_:RDFaDefaultGraph' : undef);
	
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
	
	# Next the [current element] is parsed for [URI mapping]s and these are
	# added to the [local list of URI mappings]. Note that a [URI mapping] 
	# will simply overwrite any current mapping in the list that has the same
	# name
	foreach my $A ($current_element->getAttributes)
	{
		my $attr = $A->getName;
		my $val  = $A->getValue;
		
		if ($attr =~ /^xmlns\:(.*)$/i)
		{
			$local_uri_mappings->{$1} = $val;
		}
	}
	# Mappings are provided by @xmlns. The value to be mapped is set by
	# the XML namespace prefix, and the value to map is the value of the
	# attribute - a URI. Note that the URI is not processed in any way;
	# in particular if it is a relative path it is not resolved against
	# the current [base]. Authors are advised to follow best practice
	# for using namespaces, which includes not using relative paths.
	
	# EXTENSION
	# The rdfa.info wiki includes an additional proposed syntax for defining
	# prefixes. This uses a single @prefix attribute, making it more DTD
	# friendly. Let's support that too.
	if ($current_element->getAttribute('prefix'))
	{
		foreach my $mapping ((split /\s+/, $current_element->getAttribute('prefix')))
		{
			$local_uri_mappings->{$1} = $2;
		}
	}
	
	# EXTENSION
	# KjetilK's named graphs.
	if ($this->{'named_graphs'})
	{
		if ($this->{'named_graphs'}->{'type'} eq 'id'
		&&  defined $current_element->getAttributeNS($this->{'named_graphs'}->{'xmlns'}, $this->{'named_graphs'}->{'attr'}))
		{
			$graph = $this->uri('#' . $current_element->getAttributeNS($this->{'named_graphs'}->{'xmlns'}, $this->{'named_graphs'}->{'attr'}));
		}
		elsif ($this->{'named_graphs'}->{'type'} eq 'about'
		&&  defined $current_element->getAttributeNS($this->{'named_graphs'}->{'xmlns'}, $this->{'named_graphs'}->{'attr'}))
		{
			$graph = uriOrSafeCurie($this, 
				$current_element->getAttributeNS($this->{'named_graphs'}->{'xmlns'}, $this->{'named_graphs'}->{'attr'}),
				$current_element, $local_uri_mappings);
		}
	}
	
	# The [current element] is also parsed for any language information, and
	# if present, [current language] is set accordingly.
	# Language information can be provided using the general-purpose XML
	# attribute @xml:lang .
	if (defined $current_element->getAttributeNode('xml:lang'))
		{ $current_language = $current_element->getAttribute('xml:lang'); }
	elsif (defined $current_element->getAttributeNode('lang'))
		{ $current_language = $current_element->getAttribute('lang'); }
		
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
		my $R = reservedWordOrCurie($this, $r, $current_element, $local_uri_mappings);
		push @REL, $R if (length $R);
	}
	my $rev = $current_element->getAttribute('rev');
	$rev =~ s/(^\s+|\s+$)//g;
	$rev =~ s/\s+/ /g;
	my @rev = split / /, $rev;
	my @REV;
	foreach my $r (@rev)
	{
		my $R = reservedWordOrCurie($this, $r, $current_element, $local_uri_mappings);
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
			{ $new_subject = uriOrSafeCurie($this, $current_element->getAttribute('about'), $current_element, $local_uri_mappings); }
			
		# otherwise, by using the URI from @src, if present, obtained
		# according to the section on CURIE and URI Processing.
		elsif (defined $current_element->getAttributeNode('src'))
			{ $new_subject = uriOrSafeCurie($this, $current_element->getAttribute('src'), $current_element, $local_uri_mappings); }
			
		# otherwise , by using the URI from @resource, if present, obtained
		# according to the section on CURIE and URI Processing ; 
		elsif (defined $current_element->getAttributeNode('resource'))
			{ $new_subject = uriOrSafeCurie($this, $current_element->getAttribute('resource'), $current_element, $local_uri_mappings); }
			
		# otherwise , by using the URI from @href, if present, obtained
		# according to the section on CURIE and URI Processing.
		elsif (defined $current_element->getAttributeNode('href'))
			{ $new_subject = uriOrSafeCurie($this, $current_element->getAttribute('href'), $current_element, $local_uri_mappings); }
			
		# If no URI is provided by a resource attribute, then the first
		# match from the following rules will apply: 
		else
		{
			# if the element is the head or body element then act as if
			# there is an empty @about present, and process it according to
			# the rule for @about, above; 
			if ($current_element->tagName eq 'head' || $current_element->tagName eq 'body')
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
			{ $new_subject = uriOrSafeCurie($this, $current_element->getAttribute('about'), $current_element, $local_uri_mappings); }
			
		# otherwise, by using the URI from @src, if present, obtained
		# according to the section on CURIE and URI Processing.
		elsif (defined $current_element->getAttributeNode('src'))
			{ $new_subject = uriOrSafeCurie($this, $current_element->getAttribute('src'), $current_element, $local_uri_mappings); }

		# If no URI is provided then the first match from the following rules
		# will apply: 
		else
		{
			# if the element is the head or body element then act as if
			# there is an empty @about present, and process it according
			# to the rule for @about, above; 
			if ($current_element->tagName eq 'head' || $current_element->tagName eq 'body')
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
			{ $current_object_resource = uriOrSafeCurie($this, $current_element->getAttribute('resource'), $current_element, $local_uri_mappings); }
			
		# otherwise, by using the URI from @href, if present, obtained according
		# to the section on CURIE and URI Processing.
		elsif (defined $current_element->getAttributeNode('href'))
			{ $current_object_resource = uriOrSafeCurie($this, $current_element->getAttribute('href'), $current_element, $local_uri_mappings); }
			
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
			my $rdftype = curie($this, $instanceof, $current_element, $local_uri_mappings);
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
		{ $datatype = curie($this, $current_element->getAttribute('datatype'), $current_element, $local_uri_mappings); }
		
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

		my $p = curie($this, $property, $current_element, $local_uri_mappings);
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
					$graph
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
					$graph
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

	if (defined $_[1] && defined $_[2] && defined $_[3])
	{
		push @{ $this->{'RDF'}->{$_[1]}->{$_[2]} },
		{
			'value'    => $_[3],
			'type'     => ($_[3] =~ /^_:/ ? 'bnode' : 'uri'),
		};
		
		if ($this->{'named_graphs'} && defined $_[4])
		{
			$this->{'RDF'}->{$_[1]}->{$_[2]}->[-1]->{graph} = $_[4];
			push @{ $this->{'Graphs'}->{$_[4]}->{$_[1]}->{$_[2]} },
				$this->{'RDF'}->{$_[1]}->{$_[2]}->[-1];
		}
	}
	
	$this->{'sub'}->[0]($this, @_)
		if ($this->{'sub'}->[0]);
}

sub rdf_triple_literal
# Function only used internally.
{
	my $this = shift;

	if (defined $_[1] && defined $_[2] && defined $_[3])
	{
		push @{ $this->{'RDF'}->{$_[1]}->{$_[2]} },
		{
			'value'    => $_[3],
			'type'     => 'literal',
		};

		$this->{'RDF'}->{$_[1]}->{$_[2]}->[-1]->{'datatype'} = $_[4] if $_[4];
		$this->{'RDF'}->{$_[1]}->{$_[2]}->[-1]->{'lang'}     = $_[5] if $_[5];

		if ($this->{'named_graphs'} && defined $_[6])
		{
			$this->{'RDF'}->{$_[1]}->{$_[2]}->[-1]->{graph} = $_[6];
			push @{ $this->{'Graphs'}->{$_[6]}->{$_[1]}->{$_[2]} },
				$this->{'RDF'}->{$_[1]}->{$_[2]}->[-1];
		}
	}
	
	$this->{'sub'}->[1]($this, @_)
		if ($this->{'sub'}->[1]);
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
		$rv .= $kid->toStringC14N;
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
		if ($this->{tdb} && $element && length $element->getAttribute('id'));

	return sprintf('_:RDFaAutoNode%03d', $this->{bnodes}++);
}

sub curie
# Function only used internally.
{
	my $page  = shift;
	my $str   = shift;
	my $dom   = shift;
	my $maps  = shift;
	my $xpath = $dom->getAttribute('_xpath') if ($dom);
	my $safe  = 0;
	
	# Horrible syntax for weirdo blank node.
	if ($str eq '_:' || $str eq '[_:]')
	{
		return '_:iHateThisSyntax';
	}
	
	$str = 'xhv'.$str if ($str =~ /^\:/);
	
	# Cope with safe CURIEs.
	if ($str =~ /^\[(.*)\]$/)
	{
		$str  = $1;
		$safe = 1;
	}
	
	# If the string matches CURIE syntax, then resolve the CURIE.
	if ($str =~ /^([a-z_][^\s\:]*)\:(.*)$/i)
	{
		my $pfx = $1;
		my $sfx = $2;
		my $ns  = $maps->{$pfx};
		
		return $page->uri( $ns . $sfx )
			if ($ns);
			
		return 'http://invalid.invalid/ns#'.$sfx
			if ($safe);
	}
	
	# If it wasn't a safe CURIE, then fall back to a URI.
	if (!$safe)
	{
		return $page->uri($str, {'require-absolute'=>1,'element'=>$dom});
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
	my $xpath = $dom->getAttribute('_xpath') if ($dom);
	my $safe  = 0;
	
	# Horrible syntax for weirdo blank node.
	if ($str eq '_:' || $str eq '[_:]')
	{
		return '_:iHateThisSyntax';
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
		if ($str =~ /^([a-z_][^\s\:]*)\:(.*)$/i)
		{
			my $pfx = $1;
			my $sfx = $2;
			my $ns  = $maps->{$pfx};
			
			return $page->uri( $ns . $sfx )
				if ($ns);
			
			return 'http://invalid.invalid/ns#'.$sfx
				if ($safe);
		}
	}
	
	# If it wasn't a safe CURIE, then fall back to a URI.
	else
	{
		return $page->uri($str, {'element'=>$dom});
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
	my $xpath = $dom->getAttribute('_xpath') if ($dom);
	my $safe  = 0;

	my $xhv = 'http://www.w3.org/1999/xhtml/vocab#';
	
	# Horrible syntax for weirdo blank node.
	if ($str eq '_:' || $str eq '[_:]')
	{
		return '_:iHateThisSyntax';
	}
	
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

	# Cope with safe CURIEs.
	if ($str =~ /^\[(.*)\]$/)
	{
		$str  = $1;
		$safe = 1;
	}
	
	# If the string matches CURIE syntax, then resolve the CURIE.
	if ($str =~ /^([a-z_][^\s\:]*)\:(.*)$/i)
	{
		my $pfx = $1;
		my $sfx = $2;
		my $ns  = $maps->{$pfx};
		
		return $page->uri( $ns.$sfx )
			if ($ns);
			
		return 'http://invalid.invalid/ns#'.$sfx
			if ($safe);
	}
	
	# If it wasn't a safe CURIE, then fall back to a URI.
	if (!$safe)
	{
		return $page->uri($str, {'require-absolute'=>1,'element'=>$dom});
	}
	
	return undef;
}

=item $p->graph

Returns a graph of all RDF triples found in a Perl structure close to RDF/JSON.
http://n2.talis.com/wiki/RDF_JSON_Specification

=back

=cut

sub graph
{
	my $this = shift;
	return $this->{RDF};
}

=item $p->graphs

Returns a hashref of all named graphs.

=back

=cut

sub graphs
{
	my $this = shift;
	return $this->{Graphs};
}

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
