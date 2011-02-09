use strict;
use warnings;

use inc::Module::Install;

my $dist = 'RDF-RDFa-Parser';
my $fn   = "lib/$dist.pm"; $fn =~ s#-#/#g;

name                $dist;
perl_version_from   $fn;
version_from        $fn;
abstract_from       $fn;
readme_from         $fn;
author              'Toby Inkster <tobyink@cpan.org>';
license             'perl';

requires            'common::sense'  => 0;
recommends          'Archive::Zip'   => 0;
requires            'Carp'           => 0;
requires            'Data::Dumper'   => 0;
requires            'Data::UUID'     => 0;
requires            'File::ShareDir' => 0;
test_requires       'File::Slurp'    => 0;
requires            'File::Spec'     => 0;
requires            'File::Temp'     => '0.17';
requires            'HTML::HTML5::Parser' => 0;
requires            'HTML::HTML5::Sanity' => 0;
requires            'HTTP::Cache::Transparent' => 0;
test_requires       'HTTP::Response' => 0;
requires            'LWP::UserAgent' => 0;
requires            'Module::Pluggable' => 0;
requires            'RDF::Trine'     => '0.123';
requires            'Scalar::Util'   => 0;
requires            'Storable'       => 0;
test_requires       'Test::More'     => '0.61';
requires            'URI::file'      => 0;
requires            'URI::Escape'    => 0;
requires            'URI::URL'       => 0;
requires            'XML::LibXML'    => '1.66';
requires            'XML::RegExp'    => 0;

eval 'use Archive::Zip;';
unless (UNIVERSAL::can('Archive::Zip', 'new'))
{
	print "****************\n";
	print "** WARNING\n";
	print "** Archive::Zip is needed for parsing RDFa in OpenDocument files.\n";
	print "****************\n";
}

keywords 'RDF', 'RDFa', 'Semantic Web', 'HTML', 'XHTML', 'Atom', 'SVG', 'XML', 'OpenDocument', 'ODF', 'Metadata';

resources(
	'homepage'   => "http://search.cpan.org/dist/$dist/",
	'repository' => "http://goddamn.co.uk/viewvc/perlmods/$dist/",
	'bugtracker' => "http://rt.cpan.org/Dist/Display.html?Queue=$dist",
	);

write_doap_changes;
write_doap_changes_xml;

install_share;

include 'Test::Signature';
auto_install;
WriteAll(
	'meta' => 1,
	'sign' => 1,
	);

