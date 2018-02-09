#open(FIN,"all words all features.txt");

#mac
#opendir my $dir, "//vmware-host/Shared Folders/Desktop/results" or die "Cannot open directory: $!";

#windows
opendir my $dir, "C:/Users/Doom-Lab/Desktop/ours/" or die "Cannot open directory: $!";

my @files = readdir $dir;
closedir $dir;

for ($i=2; $i<@files; $i++)
{
	open(FIN,"C:/Users/Doom-Lab/Desktop/ours/".$files[$i]) or (print "File $files[$i] not found\n");
	
while(<FIN>) {
next if /#/;
chomp; tr/ \t/ /s;
($features,$frequency) = split();
	#$short = substr($features, 0, 4);
	
	#print $short, "\n";
	
####straight frequency
	$word = $files[$i];

	
	if (not defined $features{$word}{$features})
	{
		$features{$word}{$features} = $frequency;
	}
		
	else
	{
		$features{$word}{$features}= $features{$word}{$features} + $frequency;  ###to get the total number of features listed
	}
##########


#~ ######small word frequency
	#~ if (not defined $features{$short})
	#~ {
		#~ $features{$short} = $frequency;
	#~ }
	
	#~ else
	#~ {
		#~ $features{$short} = $features{$short} + $frequency;
	#~ }
#~ ######
	
	
}
}

close(FIN);

open (FOUT, ">words_features.txt");

foreach $word (sort keys %features)
{
	foreach $features (sort keys %{$features{$word}})
	{
		print FOUT $word, " ", $features, " " , $features{$word}{$features}, "\n";
	}
}

close (FOUT);

