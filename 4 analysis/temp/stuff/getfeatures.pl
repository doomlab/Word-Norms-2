open(FIN,"all words all features.txt");
while(<FIN>) {
next if /#/;
chomp; tr/ \t/ /s;
($word, $features,$frequency) = split();

	$short = substr($features, 0, 4);
	
	#print $short, "\n";
	
#~ ####straight frequency
	
	#~ if (not defined $features{$features})
	#~ {
		#~ $features{$features} = $frequency;
	#~ }
		
	#~ else
	#~ {
		#~ $features{$features}= $features{$features} + $frequency;  ###to get the total number of features listed
	#~ }
#~ ##########


######small word frequency
	if (not defined $features{$short})
	{
		$features{$short} = $frequency;
	}
	
	else
	{
		$features{$short} = $features{$short} + $frequency;
	}
#########
	
	
}

close(FIN);

foreach $word (sort keys %features)
{
	print $word, " ", $features{$word}, "\n";
}
