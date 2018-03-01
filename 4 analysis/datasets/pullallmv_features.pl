opendir my $dir, "/Users/buchanan/OneDrive/2 projects/Word Norms 2/4 analysis/m and v data/" or die "Cannot open directory: $!";
my @files = readdir $dir;
closedir $dir;

#print "@files\n";

for ($i=2; $i<@files; $i++)
{

##create target name
($cue, $extra) = split("\\.", $files[$i]);
$where = substr($cue, 0, 1);

#print "$cue $extra\n";

##open a new file

open(FIN,"/Users/buchanan/OneDrive/2 projects/Word Norms 2/4 analysis/m and v data/".$files[$i]) or (print "File $files[$i] not found\n");

while (<FIN>){
	
	chomp;
	tr/ \t/ /s;
	tr/A-Z/a-z/;
	tr/,/ /s;

($cue, $target, $frequency) = split;

print "$where $cue $target $frequency\n";


}

}


###C:\Users\ausername\Desktop\norming work\raw frequency files\1 to do