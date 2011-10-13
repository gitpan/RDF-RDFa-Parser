#!/usr/bin/perl

use lib "lib";
use RDF::RDFa::Parser;

my $xhtml = <<XHTML;
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML+RDFa 1.0//EN"
                             "http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-GB" about="">
	<head profile="http://www.w3.org/1999/xhtml/vocab">
		<meta xname="http://search.cpan.org/dist/RDF-RDFa-Parser/#auto_config" content="xhtml_lang=1;prefix_empty=/vocab#;keywords=rdfa+html4+html32" />
	</head>
	<body typeof=":Document">
		<span property=":foo" rel="next" resource="/">Hello</span>
		<p profile="http://prefix.cc/skos,foaf,dc">
			<b typeof="foaf:Person" property="dc:title">Me&copy;</b>
		</p>
		<p vocab="http://testvocab.example.com/" typeof="Working"></p>
	</body>
</html>
XHTML

my $parser = RDF::RDFa::Parser->new(
	$xhtml, 'http://example.com/', RDF::RDFa::Parser::Config->new('xhtml','1.1'));
$parser->consume;
my $iterator = $parser->graph->as_stream;

while (my $st = $iterator->next)
{
	print $st->as_string."\n";
}

print "done\n";
