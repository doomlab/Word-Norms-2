open(FIN,"all words all features.txt");
while(<FIN>) {
next if /#/;
chomp; tr/ \t/ /s;
($word, $features,$frequency) = split();

	$short = substr($features, 0, 4);

###give each their frequency
if (not defined $wordlist{$word}{$short})
{
	$wordlist{$word}{$short} = 0;
}

$wordlist{$word}{$short} = $wordlist{$word}{$short} + $frequency;

#print $word, " ", $features, " ", $frequency, " ", $short, " ", $wordlist{$word}{$short}, "\n";

###create total frequency for each word
if (not defined $total{$word})
{
	$total{$word} = 0;
	#print $word, " ", $total{$word}, " \n";
}

$total{$word} = $frequency + $total{$word};
#print $total{$word}, " ", $frequency, " \n";

}


##create percentages of each feature set
foreach $word (sort keys %wordlist)
{
	foreach $feature (sort keys %{$wordlist{$word}})
	{
		$percentage{$word}{$feature} = $wordlist{$word}{$feature} / $total{$word};
	}
}

foreach $word (sort keys %percentage)
{
	foreach $feature (sort keys %{$percentage{$word}})
	{
		if ($percentage{$word}{$feature} > 0.049)
		{
			print $word, " ", $feature, " ", $wordlist{$word}{$feature}, "\n";
		}
	}
}
		







#~ ######small word frequency
	#~ if (not defined $features{$short})
	#~ {
		#~ $features{$short} = $frequency;
	#~ }
	
	#~ else
	#~ {
		#~ $features{$short} = $features{$short} + $frequency;
	#~ }
#~ #########
	
	
#~ }

#~ close(FIN);

#~ foreach $word (sort keys %features)
#~ {
	#~ print $word, " ", $features{$word}, "\n";
#~ }


