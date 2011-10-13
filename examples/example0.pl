 use Time::HiRes qw[time];

 use RDF::RDFa::Parser;
 my $url     = 'http://www.rottentomatoes.com/m/net/';
 $S = time();
 my $options = RDF::RDFa::Parser::Config->tagsoup;
 my $rdfa    = RDF::RDFa::Parser->new_from_url($url, $options);
 $M = time();
 print $rdfa->opengraph('title') . "\n";
 print $rdfa->opengraph('image') . "\n";
 $E = time();

print (($E-$S) . "\n");
print (($E-$M) . "\n");
