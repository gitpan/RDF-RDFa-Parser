package TestSuite;

use File::Slurp;
use File::ShareDir qw(dist_file);
use HTML::HTML5::Parser;
use HTML::HTML5::Sanity;
use RDF::Query;
use RDF::Trine;
use Test::More;
use XML::LibXML;
use strict;

sub doit
{
my ($flavour) = @_;

$RDF::Trine::Node::Literal::USE_XMLLITERALS = 0;
$| = 1;

die "This ($flavour) tastes funny!\n"
	unless $flavour =~ /^(xhtml1|html4|html5)$/;

my $html_parser;
if ($flavour =~ /xhtml/)
{
	$html_parser = XML::LibXML->new;
	$html_parser->load_catalog( dist_file('RDF-RDFa-Parser', 'catalogue/index.xml') );
}
else
{
	$html_parser = HTML::HTML5::Parser->new;
}

my $rdfa_parser_options;
if ($flavour =~ /xhtml/)
{
	$rdfa_parser_options = RDF::RDFa::Parser::Config->new('xhtml', '1.0');
}
elsif ($flavour =~ /html4/)
{
	$rdfa_parser_options = RDF::RDFa::Parser::Config->new('html4', '1.0');
}
elsif ($flavour =~ /html5/)
{
	$rdfa_parser_options = RDF::RDFa::Parser::Config->new('html5', '1.0');
}

my $skip = shift @ARGV || 0;
my $andstop = 0;
if ($skip =~ /^=(.*)$/)
{
	$skip = $1;
	$andstop = 1;
}

# This is where we'll store our results.
my $R = {};
my $PASS = my $FAIL = my $SKIP = 0;

# Parse the test case manifest.
diag("Parsing $flavour manifest...\n");
my $manifest = RDF::Trine::Model->new( RDF::Trine::Store->temporary_store );
my $m_rdfxml = read_file('t/' . $flavour . "-manifest.rdf");
my $rdfxml_p = RDF::Trine::Parser::RDFXML->new;
$rdfxml_p->parse_into_model(
	"http://example.com/",
	$m_rdfxml,
	$manifest);

# Query it to get a list of approved tests.
diag("Querying manifest...\n");
my $manifest_sparql = <<ENDSPARQL;

PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX test: <http://www.w3.org/2006/03/test-description#>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
SELECT *
WHERE {
	?testcase
		a test:TestCase ;
		dc:title ?title ;
		test:reviewStatus test:approved ;
		test:informationResourceInput ?input ;
		test:informationResourceResults ?results ;
		test:purpose ?purpose .
	OPTIONAL { ?testcase test:expectedResults ?expected . }
}
ORDER BY ?testcase

ENDSPARQL

my $manifest_query = RDF::Query->new($manifest_sparql);
my $iterator       = $manifest_query->execute($manifest);

# For each approved test...
while (my $TC = $iterator->next)
{
	# Just need these as plain old strings.
	my $testcase = $TC->{'testcase'}->uri;
	my $input    = $TC->{'input'}->uri;
	my $results  = $TC->{'results'}->uri;
	my $title    = $TC->{'title'}->literal_value if $TC->{'title'};
	my $purpose  = $TC->{'purpose'}->literal_value if $TC->{'purpose'};
	my $expected = lc $TC->{'expected'}->literal_value if $TC->{'expected'};
	
	$expected = 'true' unless defined $expected && $expected eq 'false';
	
	# SELECT DISTINCT(?testcase)
	# i.e. if we have already got a result for this test, skip it.
	next if defined $R->{$testcase};

	my $testn = substr($testcase, (length $testcase)-4);
	if ($testn < $skip)
	{
		next;
	}
	elsif (($testn > $skip) && $andstop)
	{
		last;
	}
	
	# Use local copy of test suite.
	$input =~ s/\.html$/.xhtml/;
	my ($f_input, $f_results) = ($input, $results);
	$f_input   =~ s|^http://rdfa.digitalbazaar.com/test-suite/test-cases/$flavour/(.*)\.xhtml|tests/$1.txt|;
	$f_results =~ s|^http://rdfa.digitalbazaar.com/test-suite/test-cases/$flavour/(.*)\.sparql|tests/$1.sparql|;
	my @text   = read_file('t/'.$f_input);
	my $sparql = read_file('t/'.$f_results);
	
	# Local (X)HTML copy is flavour-neutral. Need to generate real XHTML/HTML from text.
	my ($namespaces, $html) = ('', undef);
	while ($text[0] !~ /^\s*\<head/)
	{
		$namespaces .= shift @text;
	}
	if ($flavour eq 'xhtml1')
	{
		$html = q(<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML+RDFa 1.0//EN" "http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd">) . "\n"
		      . q(<html xmlns="http://www.w3.org/1999/xhtml" version="XHTML+RDFa 1.0") . "\n"
		      . $namespaces . ">\n"
		      . (join '', @text)
		      . q(</html>) . "\n";
	}
	elsif ($flavour eq 'html4')
	{
		$html = q(<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">) . "\n"
		      . q(<html version="HTML+RDFa 1.0") . "\n"
		      . $namespaces . ">\n"
		      . (join '', @text)
		      . q(</html>) . "\n";
	}
	elsif ($flavour eq 'html5')
	{
		$html = q(<!doctype html>) . "\n"
		      . q(<html version="HTML+RDFa 1.0") . "\n"
		      . $namespaces . ">\n"
		      . (join '', @text)
		      . q(</html>) . "\n";
	}
	
	# Local copies needs $TCPATH substituting.
	$sparql =~ s|\$TCPATH|http://rdfa.digitalbazaar.com/test-suite/test-cases/$flavour|g;
	$html   =~ s|\$TCPATH|http://rdfa.digitalbazaar.com/test-suite/test-cases/$flavour|g;

	SKIP:
	{

	if ($testcase eq 'http://rdfa.digitalbazaar.com/test-suite/test-cases/html4/0012'
	||  $testcase eq 'http://rdfa.digitalbazaar.com/test-suite/test-cases/html4/0013'
	||  $testcase eq 'http://rdfa.digitalbazaar.com/test-suite/test-cases/html4/0101'
	||  $testcase eq 'http://rdfa.digitalbazaar.com/test-suite/test-cases/html4/0102'
	||  $testcase eq 'http://rdfa.digitalbazaar.com/test-suite/test-cases/html4/0108'
	||  $testcase eq 'http://rdfa.digitalbazaar.com/test-suite/test-cases/html4/0113'
	||  $testcase eq 'http://rdfa.digitalbazaar.com/test-suite/test-cases/html4/0121'
	||  $testcase eq 'http://rdfa.digitalbazaar.com/test-suite/test-cases/html5/0113'
	||  $testcase eq 'http://rdfa.digitalbazaar.com/test-suite/test-cases/html5/0121')
	{
		$SKIP++;
		skip("Skipping rubbish test - $testcase", 1);
	}
	
	# Try parsing the test case and querying it with reference SPARQL.
	my ($model, $ask_result, $dom);	
	eval {
		
		if ($flavour !~ /xhtml/)
		{
			my $html5dom = $html_parser->parse_string($html);
			$dom = HTML::HTML5::Sanity::fix_document($html5dom);
		}
		else
		{
			$dom = $html_parser->parse_string($html);
		}
		
		my $rdfa_parser = RDF::RDFa::Parser->new($dom, $input, $rdfa_parser_options);
		$rdfa_parser->consume;
		$rdfa_parser->set_callbacks({onerror=>sub{}});
		$model = $rdfa_parser->graph;
		
		my $query   = RDF::Query->new($sparql);
		$ask_result = $query->execute($model);
	};
	
	$R->{$testcase} = {
		'title'    => $title,
		'purpose'  => $purpose,
		'model'    => $model,
		'expected' => $expected,
		'testcase' => $testcase,
		};
	
	# Decide whether we got a pass or fail.
	if (!defined $dom)
	{
		$R->{$testcase}->{'result'} = 'dom-error';
	}
	elsif (!defined $ask_result)
	{
		$R->{$testcase}->{'result'} = 'eval-error';
	}
	elsif (!$ask_result->is_boolean)
	{
		$R->{$testcase}->{'result'} = 'sparql-error';
	}
	else
	{
		$R->{$testcase}->{'result'} = $ask_result->get_boolean ? 'true' : 'false';
	}
	
	$R->{$testcase}->{'conclusion'} = ($R->{$testcase}->{'result'} eq $R->{$testcase}->{'expected'}) ? 'PASS' : 'FAIL' ;
	
	if ($R->{$testcase}->{'result'} eq 'eval-error')
	{
		$FAIL++;
		ok(0, "$testcase ".$R->{$testcase}->{'title'});
	}
	elsif ($R->{$testcase}->{'result'} eq 'dom-error')
	{
		$FAIL++;
		ok(0, "$testcase ".$R->{$testcase}->{'title'});
		diag(document_to_clarkml($dom) . "\n") if $dom;
	}
		
	if ($R->{$testcase}->{'result'} eq $R->{$testcase}->{'expected'})
	{
		$PASS++;
		ok(1, "$testcase ".$R->{$testcase}->{'title'});
	}
	else
	{
		$FAIL++;
		ok(0, "$testcase ".$R->{$testcase}->{'title'});
		if ($model)
		{
			diag("  Model generated:\n");
			my $iter = $model->as_stream;
			while (my $st = $iter->next)
			{
				diag("    " . $st->as_string . "\n");
			}
		}
	}

	}	
}

diag(sprintf("Passed %d, failed %d, skipped %s out of total %d tests.\n", $PASS, $FAIL, $SKIP, $SKIP+$PASS+$FAIL));
done_testing($PASS+$FAIL+$SKIP);

}

1;
