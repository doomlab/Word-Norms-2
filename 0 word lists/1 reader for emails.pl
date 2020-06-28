#~ From: Apps@MissouriState.edu
#~ Date: December 1, 2010 9:48:07 PM CST
#~ To: buchananlab@gmail.com
#~ Subject: SEMANTICS MONTANA
#~ aggieerin@gmail.com

#~ Semantic Data  WL14 MONTANA 
#~ Practice:


for ($i=801; $i<900; $i++)
{
$fileopen = "S".$i.".txt";

open(FIN,$fileopen) or (print "File $fileopen not found\n");
while (<FIN>){
	next if /^Trial/;
	next if /Semantic/;
	next if /Practice/;
	next if /Experimental/;
	next if /-/;
	next if /Theme/;
	next if /heme/;
	next if /^Webserver/;
	next if /aggieerin/;
	next if /^From:/;
	next if /^Date:/;
	next if /^To:/;
	next if /^Subject:/;
	
	chomp;
	tr/ \t/ /s;
	tr/A-Z/a-z/;

	

($number, $target, @line) = split;

for ($r=0; $r<@line; $r++)
{
	$words{$target}{$i}{$r} = $line[$r];
	print $line[$r], "\n";
}
}
}

foreach $target (sort keys %words)
{
	open (FOUT, ">$target.txt");
	
	foreach $person (sort keys %{$words{$target}})
	{
		foreach $lines (sort keys %{$words{$target}{$person}})
		{
			print FOUT $words{$target}{$person}{$lines}, " ";
		}
		
		print FOUT "\n";
	}
	
	close (FOUT);
}

	
