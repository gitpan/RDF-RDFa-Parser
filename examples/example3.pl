#!/usr/bin/perl

use lib "lib";
use RDF::RDFa::Parser;

my $xhtml = <<XHTML;
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML+RDFa 1.0//EN"
                             "http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-GB" about=""
	xmlns:text="http://foo.com/">
	<meta name="http://search.cpan.org/dist/RDF-RDFa-Parser/#auto_config" content="xhtml_lang=1;prefix_empty=/vocab#;keywords=rdfa+html4+html32+iana" />
	<meta name="http://search.cpan.org/dist/RDF-RDFa-Parser/#auto_config" content="bookmark_start={http://foo.com/}start;bookmark_end=>{http://foo.com/}end;bookmark_name={http://foo.com/}name" />
	<text:start text:name="sample" property=":sample" />
	<span property=":foo" rel="next self" resource="/">Hello</span>
	<p profile="http://www.w3.org/2007/08/pyRdfa/profiles/basic.rdf">
		<b typeof="g:Person" property="g:name">Bob</b>
		<text:end text:name="notsample"/>
		<i profile="http://www.w3.org/2007/08/pyRdfa/profiles/foaf.html">
			<b typeof="Person" property="name g:name">Alice</b>
			<text:end text:name="sample"/>
		</i>
	</p>
</html>
XHTML

my $parser = RDF::RDFa::Parser->new(
	$xhtml, 'http://example.com/',
	RDF::RDFa::Parser::Config->new('xhtml','1.1', auto_config => 1));
$parser->consume;
my $iterator = $parser->graph->as_stream;

while (my $st = $iterator->next)
{
	print $st->as_string."\n";
}

print "done\n";
