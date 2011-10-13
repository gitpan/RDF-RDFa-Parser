#!/usr/bin/perl

use 5.010;
use lib "lib";
use RDF::RDFa::Parser;
use RDF::TrineShortcuts;

my $xhtml = <<XHTML;
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML+RDFa 1.0//EN"
                             "http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-GB" about="">
	<head profile="http://www.w3.org/1999/xhtml/vocab">
		<meta xname="http://search.cpan.org/dist/RDF-RDFa-Parser/#auto_config" content="xhtml_lang=1;prefix_empty=/vocab#;keywords=rdfa+html4+html32" />
	</head>
	<body typeof=":Document">
		<span property=":foo" rel="next" resource="/">Hello</span>
		<p profile="http://prefix.cc/skos,foaf,dc" rel=":x">
			<b typeof="foaf:Person" property="dc:title">Me&copy;</b>
		</p>
		<div vocab="http://testvocab.example.com/" typeof="Working"></div>
		<blockquote role=":foo" cite="#bar">Baz</blockquote>
		<div about="#t2"
		     prefix="ex1: http://example.com/1# ex2: http://example.com/2# ex3:
			http://example.com/3#"
		     rel="ex1:test ex2:test ex3:test ex4:test" resource="#t3"></div> 

		<p prefix="bibo: http://purl.org/ontology/bibo/ dc: http://purl.org/dc/terms/" typeof="bibo:Chapter">
		  "<span property="dc:title">Semantic Annotation and Retrieval</span>", by
		  <span inlist="" property="dc:creator">Ben Adida</span>,
		  <span inlist="" property="dc:creator">Mark Birbeck</span>, and
		  <span inlist="" property="dc:creator">Ivan Herman</span>.
		</p>

		<div rel="rdfa:testing" inlist="">
			<span about="#f1"></span>
			<span resource="#f2"></span>
		</div>

                <div rev="rdfa:testing2" inlist="">
                        <span about="#f3"></span>
                        <span resource="#f4"></span>
                </div>


	</body>
</html>
XHTML

my $parser = RDF::RDFa::Parser->new(
	$xhtml, 'http://example.com/', RDF::RDFa::Parser::Config->new('xhtml','1.1', role_attr=>1, cite_attr=>1, profile_attr=>1));
$parser->set_callbacks({
	ontriple => sub {
		my ($self, $elems, $statement) = @_;
		while (my ($k,$v) = each %$elems)
		{
			print "# $k => ".$v->tagName . "\n" if defined $v;
		}
		print join ' ', map { $_->as_ntriples } $statement->nodes;
		print " .\n\n";
		return 0;
	}
	});
$parser->consume;

say rdf_string($parser->graph => 'Turtle');

#my $iterator = $parser->graph->as_stream;
#
#while (my $st = $iterator->next)
#{
#	print $st->as_string."\n";
#}
#
#print "done\n";
