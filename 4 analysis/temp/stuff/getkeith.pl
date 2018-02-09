#~ ####our cosine values
for ($r = 1; $r<6; $r++)
{
open(FIN,"all cosine values".$r.".txt");
while(<FIN>) {
next if /#/;
chomp; tr/ \t/ /s;
($worda, $wordb, $cosine) = split();

$word1 = lc($worda);
$word2 = lc($wordb);

$w1 = substr($word1, 2);
$w2 = substr($word2, 2);

if (not defined $cos{$w1}{$w2})
{
	$cos{$w1}{$w2} = $cosine;
}
else
{
	$cos{$w1}{$w2} = $cos{$w1}{$w2} + $cosine;
	
	if (not defined $totals{$w1}{$w2})
	{
		$totals{$w1}{$w2} = 1;
	}
	
	$totals{$w1}{$w2} = $totals{$w1}{$w2} + 1;
}
}

close(FIN);
}

foreach $w1 (sort keys %totals)
{
	foreach $w2 (sort keys %{$totals{$w1}})
	{
		$cos{$w1}{$w2} = $cos{$w1}{$w2} / $totals{$w1}{$w2};
		
		#print "$w1 $w2 $cos{$w1}{$w2} $totals{$w1}{$w2} \n";
	}
}

open(FIN,"keithlist.txt");
while(<FIN>) {
next if /#/;
chomp; tr/ \t/ /s;
($worda, $wordb, $wordc) = split();

$word1 = lc($worda);
$word2 = lc($wordb);
$word3 = lc($wordc);

if (not defined $cos{$word3}{$word1})
{
	$cos{$word3}{$word1} = "null";
}

if (not defined $cos{$word3}{$word2})
{
	$cos{$word3}{$word2} = "null";
}


print $word1, " ", $word2, " ", $word3, " ", $cos{$word3}{$word1}, " ", $cos{$word3}{$word2}, "\n";
}
