use lib '/Users/buchanan/perl5/lib/perl5/';

use WordNet::Similarity::jcn;

use WordNet::QueryData;

$data[0] = "abuse#v#1";
$data[1] = "use#v#1";

my $wn = WordNet::QueryData->new();

my $rel = WordNet::Similarity::jcn->new($wn);

my $value = $rel->getRelatedness($data[0], $data[1]);

($error, $errorString) = $rel->getError();

if($error) { print "$data[0] $data[1] $errorString\n";}

else {print "$data[0] <-> $data[1] = $value\n";}

