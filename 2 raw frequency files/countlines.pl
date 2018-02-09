#mac
#opendir my $dir, "//vmware-host/Shared Folders/Desktop/results" or die "Cannot open directory: $!";

#windows
opendir my $dir, "C:/Users/Doom-Lab/Desktop/ours/" or die "Cannot open directory: $!";

my @files = readdir $dir;
closedir $dir;

print "@files";

for ($i=2; $i<@files; $i++)
{
	open(FIN,"C:/Users/Doom-Lab/Desktop/ours/".$files[$i]) or (print "File $files[$i] not found\n");
	
	$word = $files[$i];
	
	while(<FIN>) {
		next if /#/;
		chomp; tr/ \t/ /s;
		@line = split();
		#print "$word @line\n";
		
		$number{$word} = $number{$word}+1
	}


}

foreach $word (sort keys %number)
{
	print $word, " ", $number{$word}, "\n";
}