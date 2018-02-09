#~ ####our cosine values
#for ($r = 1; $r<6; $r++)
#{
#~ open(FIN,"all cosine values".$r.".txt");
open(FIN,"all cosine values 4 chars.txt");
while(<FIN>) {
next if /#/;
chomp; tr/ \t/ /s;
($worda, $wordb, $cosine) = split();

$word1 = lc($worda);
$word2 = lc($wordb);

$w1 = substr($word1, 2);
$w2 = substr($word2, 2);

if ($w1 eq $w2) {
	print $w1, " ", $w2, " ", $word1, " ", $word2, " ", $cosine, "\n";
}
}

close(FIN);

#}

