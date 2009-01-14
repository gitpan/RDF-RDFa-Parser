# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl RDF-RDFa-Parser.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 5;
BEGIN { use_ok('RDF::RDFa::Parser') };

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

use RDF::RDFa::Parser;

my $xhtml = <<EOF;
<html xmlns:dc="http://purl.org/dc/terms/" xmlns:foaf="http://xmlns.com/foaf/0.1/" xml:lang="en">
	<head>
		<title property="dc:title">This is the title</title>
	</head>
	<body xmlns:dc="http://purl.org/dc/elements/1.1/">
		<div rel="foaf:primaryTopic" rev="foaf:page" xml:lang="de">
			<h1 about="#topic" typeof="foaf:Person" property="foaf:name">Albert Einstein</h1>
		</div>
		<address rel="foaf:maker dc:creator" rev="foaf:made">
			<a about="#maker" property="foaf:name" rel="foaf:homepage" href="joe">Joe Bloggs</a>
		</address>
	</body>
</html>
EOF
$parser = RDF::RDFa::Parser->new($xhtml, 'http://example.com/einstein');
$parser->set_callbacks(undef, sub{
	if ($_[2] eq 'http://example.com/einstein#maker'
	&&  $_[3] eq 'http://xmlns.com/foaf/0.1/name')
	{
		ok($_[4] eq 'Joe Bloggs', 'Callbacks working OK.');
	}
});
ok(lc($parser->dom->documentElement->tagName) eq 'html', 'DOM Tree returned OK.');
$parser->consume;
my $graph = $parser->graph;

ok($graph->{'http://example.com/einstein'}->{'http://purl.org/dc/elements/1.1/creator'}->[0]->{'value'} eq 'http://example.com/einstein#maker' , 'RDFa graph looks OK (tested a resource).');
ok($graph->{'http://example.com/einstein#topic'}->{'http://xmlns.com/foaf/0.1/name'}->[0]->{'value'} eq 'Albert Einstein' , 'RDFa graph looks OK (tested a literal).');
