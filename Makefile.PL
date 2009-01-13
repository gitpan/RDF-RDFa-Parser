use 5.008006;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'RDF::RDFa::Parser',
    VERSION_FROM      => 'lib/RDF/RDFa/Parser.pm', # finds $VERSION
    PREREQ_PM         => { 'XML::LibXML' => 1.66 , 'URI::URL' => 5 }, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/RDF/RDFa/Parser.pm', # retrieve abstract from module
       AUTHOR         => 'Toby Inkster <tai@apple.com>') : ()),
);
