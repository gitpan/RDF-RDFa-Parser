#!/usr/bin/perl

use lib "lib";
use RDF::RDFa::Parser;

my $xml = <<'XML';
<?xml version="1.0" encoding="UTF-8"?>
<?profile http://search.yahoo.com/searchmonkey-profile ?>
<feed xmlns="http://www.w3.org/2005/Atom" xmlns:y="http://search.yahoo.com/datarss/">
  <id>http://argot-hub.googlecode.com/svn/trunk/_samples/hmrc/datarss-assistant-officer.xml</id>
  <author>
    <name>HMRC</name>
  </author>
  <title type="text">HMRC's vacancies feed</title>
  <updated>2008-07-07T18:30:02Z</updated>

  <entry>
    <id>http://www.hmrc.gov.uk/jobs/psr/assistant-officer.htm</id>
    <title type="text">Assistant Officer</title>
    <content type="application/xml">
      <y:adjunctcontainer>
        <y:adjunct version="1.0" name="employer">
          <y:item rel="rel:Employer">
            <y:meta property="vcard:fn">HMRC</y:meta>
            <y:item rel="vcard:url" resource="http://www.hmrc.gov.uk/"/>
            <y:item rel="rel:Thumbnail" resource="http://www.hmrc.gov.uk/images/logo.gif">
              <y:meta property="media:width">160</y:meta>
              <y:meta property="media:height">62</y:meta>
            </y:item>
          </y:item>
        </y:adjunct>

        <y:adjunct version="1.0" name="vacancy">
          <y:item xmlns:v="http://code.google.com/p/argot-hub/wiki/ArgotVacancy#" rel="rel:Listing">
            <y:meta property="dc:identifier">CN-EXT-15-08</y:meta>
            <y:meta property="dc:title">Assistant Officer</y:meta>
            <y:meta property="dc:description">
              Analysing data and process applications in given timescales, liasing with customers
              and their representatives, working with colleagues to achieve team targets. In
              addition to all of the other benefits that come with working with us, you will
              enjoy 22 days annual leave, rising to 25 after a year, plus 10.5 public and privilege
              days and access to a civil service pension scheme.
            </y:meta>
            <y:meta property="geo:location">Wolverhampton</y:meta>
            <y:meta property="job:salaryType">annual</y:meta>
            <y:meta property="job:salaryFrom" datatype="currency:GBP">15262</y:meta>
            <y:meta property="job:salaryTo" datatype="currency:GBP">15262</y:meta>
            <y:meta property="job:hireType">full-time</y:meta>

            <y:meta property="v:closingDate">2008-08-08</y:meta>
          </y:item>
        </y:adjunct>
      </y:adjunctcontainer>
    </content>
  </entry>
</feed>
XML

my $parser = RDF::RDFa::Parser->new(
	$xml,
	'http://example.net/',
	RDF::RDFa::Parser::Config->new('datarss'));
my $iterator = $parser->graph->as_stream;

while (my $st = $iterator->next)
{
	print $st->as_string."\n";
}

print "done\n";
