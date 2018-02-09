opendir my $dir, "/Users/ausername/Desktop/norming work backup/raw word files" or die "Cannot open directory: $!";
my @files = readdir $dir;
closedir $dir;


for ($i=2; $i<@files; $i++)
{

##create target name
($cue, $extra) = split("\\.", $files[$i]);
##open a new file


open(FIN,"/Users/ausername/Desktop/norming work backup/raw word files/".$files[$i]) or (print "File $files[$i] not found\n");

while (<FIN>){
	
	chomp;
	tr/ \t/ /s;
	tr/A-Z/a-z/;
	tr/,/ /s;

@lines = split();

$newstuff = @lines;

if (not defined $total{$cue})
{
	$total{$cue} = 0;
}

$total{$cue} = $total{$cue} + $newstuff;

}

close (FIN);

}

foreach $word (sort keys %total)
{
	print $word, " ", $total{$word}, "\n";
} 



###C:\Users\ausername\Desktop\norming work\raw frequency files\1 to do