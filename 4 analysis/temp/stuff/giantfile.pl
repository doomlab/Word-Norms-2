opendir my $dir, "E:/norming r2/norming stuff/c raw frequency files/cleaned up files/" or die "Cannot open directory: $!";
my @files = readdir $dir;
closedir $dir;



for ($i=0; $i<@files; $i++)
{
	print $files[$i], "\n";
}

open (FOUT, ">>E:/norming r2/norming stuff/d edited frequency files/all_features_unedit.txt");


for ($i=2; $i<@files; $i++)
{

open(FIN,"E:/norming r2/norming stuff/c raw frequency files/cleaned up files/".$files[$i]) or (print "File $files[$i] not found\n");

while (<FIN>){
	
	chomp;
	tr/ \t/ /s;
	tr/A-Z/a-z/;
	tr/,/ /s;

($word1, $word2, $frequency) = split;
##pull in the crap in these files

print FOUT $word1, " ", $word2, " ", $frequency, " \n";

}
close (FIN);
}

close (FOUT);

#~ ###C:\Users\ausername\Desktop\norming work\raw frequency files\1 to do