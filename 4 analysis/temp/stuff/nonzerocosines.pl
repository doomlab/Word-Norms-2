#~ ####our cosine values
for ($r = 1; $r<21; $r++)
{
open(FIN,"all cosine values long".$r.".txt");
while(<FIN>) {
next if /#/;
chomp; tr/ \t/ /s;
($worda, $wordb, $cosine) = split();

#print $worda, " ", $wordb, " ", $cosine, "\n";
next if ($cosine == 0);

next if ($worda =~ m/^o_/);
next if ($wordb =~ m/^o_/);
next if ($cosine < .049);
next if ($cosine == 1);

#next if ($worda =~ m/^m_/);
#next if ($wordb =~ m/^m_/);
#next if ($worda =~ m/^v_/);
#next if ($wordb =~ m/^v_/);

#print "YAY $worda\n";

$word1 = lc($worda);
$word2 = lc($wordb);

#$w1 = lc($worda);
#$w2 = lc($wordb);



$w1 = substr($word1, 2);
$w2 = substr($word2, 2);

if (not defined $cos{$w1}{$w2})
{
	$cos{$w1}{$w2} = $cosine;
	$totals{$w1}{$w2} = 1;
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


foreach $w1 (sort keys %cos)
{
	foreach $w2 (sort keys %{$cos{$w1}})
	{
		$cos{$w1}{$w2} = $cos{$w1}{$w2} / $totals{$w1}{$w2};
		
		print "$w1 $w2 $cos{$w1}{$w2} $totals{$w1}{$w2} \n";
	}
}
