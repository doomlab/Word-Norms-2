$fileopen = "wl5.5 words.txt";

open(FIN,$fileopen) or (print "File $fileopen not found\n");
while (<FIN>){

	chomp;
	tr/ \t/ /s;
	tr/A-Z/a-z/;
	tr/,/ /s;

($target, @line) = split;

print $target, " \n";

open (FOUT, ">>$target.txt");

for ($i=0;$i<@line;$i++)
{
	print FOUT $line[$i], " ";
}

print FOUT "\n";
close (FOUT);
	}
