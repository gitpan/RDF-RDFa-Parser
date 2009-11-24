use 5.008006;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME              => 'RDF::RDFa::Parser',
    VERSION_FROM      => 'lib/RDF/RDFa/Parser.pm',
    PREREQ_PM         => { 'XML::LibXML' => 1.66 , 'URI::URL' => 5 , 'RDF::Trine'=>'0.111' }, 

    ($] >= 5.005 ?
      (ABSTRACT_FROM  => 'lib/RDF/RDFa/Parser.pm',
       AUTHOR         => 'Toby Inkster <tai@g5n.co.uk>') : ()),
);
