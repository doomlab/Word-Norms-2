opendir my $dir, "//vmware-host/Shared Folders/Documents/RESEARCH/1 MSU/2 writing/norming/data/raw word files" or die "Cannot open directory: $!";
my @files = readdir $dir;
closedir $dir;

#~ for ($i=0; $i<@files; $i++)
#~ {
	#~ print $files[$i], "\n";
#~ }


for ($i=3; $i<@files; $i++)
{

open(FIN,"//vmware-host/Shared Folders/Documents/RESEARCH/1 MSU/2 writing/norming/data/raw word files/".$files[$i]) or (print "File $files[$i] not found\n");

while (<FIN>){
	
	chomp;
	tr/ \t/ /s;
	tr/A-Z/a-z/;
	tr/,/ /s;

(@features) = split;
##pull in the crap in these files

$cheese = @features;

if (not defined $totalcount{$files[$i]})
{
	$totalcount{$files[$i]} = 0;
	$subjects{$files[$i]} = 0;
}

$totalcount{$files[$i]} = $totalcount{$files[$i]} + $cheese;
$subjects{$files[$i]}++;


print $cheese, " ", $totalcount{$files[$i]}, " ", $subjects{$files[$i]}, " ", $files[$i], "\n";


}
close (FIN);
}


foreach $word (sort keys %totalcount)
{
	print $word, " ", $totalcount{$word}/$subjects{$word}, " \n";
}


#~ ###C:\Users\ausername\Desktop\norming work\raw frequency files\1 to do