
####first open the list of current exception combinations

$fileopen = "exception_words.txt";

open(FIN,$fileopen) or (print "File $fileopen not found\n");
while (<FIN>){

	chomp;
	tr/ \t/ /s;
	tr/A-Z/a-z/;
	tr/,/ /s;

($word1, $word2) = split;

#print $word1, " ", $word2, " \n";

###word 1 = base word
###word 2 = word to correct

$exceptions{$word2} = $word1;

	}
close (FIN);


#####pull in added exception words
$fileopen = "exceptions_added.txt";

open(FIN,$fileopen) or (print "File $fileopen not found\n");
while (<FIN>){

	chomp;
	tr/ \t/ /s;
	tr/A-Z/a-z/;
	tr/,/ /s;

($word1, $word2) = split;

#print $word1, " ", $word2, " \n";

###word 1 = base word
###word 2 = word to correct

$exceptions{$word2} = $word1;

	}
close (FIN);


####pull in checked words
$fileopen = "checked.txt";

open(FIN,$fileopen) or (print "File $fileopen not found\n");
while (<FIN>){

	chomp;
	tr/ \t/ /s;
	tr/A-Z/a-z/;
	tr/,/ /s;

($word1, $word2) = split;

#print $word1, " ", $word2, " \n";

###word 1 = base word
###word 2 = word to correct

$checked{$word1}{$word2}= 1;

	}
close (FIN);



######then pull in a file or files to process

######run each file separately (take out # to make this run)#######
#opendir my $dir, "/Users/ausername/Desktop/2 word counter/results/" or die "Cannot open directory: $!";
###change this to point to your directory ######
# my @files = readdir $dir;
# closedir $dir;

####run one giant file of all features 
$fileopen2 = "all_features_unedit.txt";

open(FIN,$fileopen2) or (print "File $fileopen not found\n");
while (<FIN>){

($cue, $target, $frequency) = split();

#print $cue, " ", $target, " ", $frequency, " \n";

####Make sure the frequencies exist - weeds out the bad lines without all three numbers.
if ($frequency =~ /\d/)
{
	####this pulls all the words into one giant hash, so you can use the keys for target
	$featurelist{$cue}{$target} = $frequency;
	push (@features, $target);

}


}

close (FIN);


####regular expressions to add to the list



for ($k = 0; $k<@features; $k++)
{
	$short = substr($features[$k], 0, 4);
	
	for ($e = 0; $e<@features; $e++)
	{
		
		if (not defined $checked{$features[$k]}{$features[$e]} or not defined $checked{$features[$e]}{$features[$k]})
		{
			
			if($features[$k] eq $features[$e])
			{
				###place holder so it doesn't ask you about the same words
				$checked{$features[$k]}{$features[$e]} = 1;
				$checked{$features[$e]}{$features[$k]} = 1;
				
				open (FOUT, ">>checked.txt");

				print FOUT $features[$e], " ", $features[$k], "\n";
				print FOUT $features[$k], " ", $features[$e], "\n";
				
				close (FOUT);
			}
			
			elsif ($features[$e] =~ m/$short/)
			{
				print $features[$k], " ", $features[$e], "\n";
				
				###first check if it's already in the exceptions list
				if ($exceptions{$features[$k]} eq $features[$e] or $exceptions{$features[$e]} eq $features[$k])
				{
					####place holder and move on
					print "$features[$k] $features[$e] already exist\n\n";
				}
			
				elsif ($features[$k]."s" eq $features[$e] or $features[$k]."ed" eq $features[$e] or $features[$k]."ing" eq $features[$e] or $features[$k]."ly" eq $features[$e] or $features[$k]."est" eq $features[$e] or $features[$k]."less" eq $features[$e] or $features[$k]."ness" eq $features[$e] or $features[$k]."ment" eq $features[$e])
				{
					print "Does this combination look ok?\n base: $features[$k]\n fix: $features[$e]\n";
					chomp ($combine = <>);
			
					if ($combine eq 'y' or $combine eq 'yes')
					{
						$exceptions{$features[$e]} = $features[$k];
						
						open (FOUT, ">>exceptions_added.txt");
						print FOUT $features[$k], " ", $features[$e], "\n";
						close (FOUT);
					}
						
					$checked{$features[$k]}{$features[$e]} = 1;
					$checked{$features[$e]}{$features[$k]} = 1;
					
					open (FOUT, ">>checked.txt");
					print FOUT $features[$e], " ", $features[$k], "\n";
					print FOUT $features[$k], " ", $features[$e], "\n";
					close (FOUT);
					
				}
			
				else
				{
					print "would you like to combine these two words?\n";
					chomp ($combine = <>);
				
					if ($combine eq 'y' or $combine eq 'yes')
					{
						print "What is the base word?\n";
						chomp ($base = <>);
						print "What is the word to fix?\n";
						chomp ($fix = <>);
					
						$exceptions{$fix} = $base;
						open (FOUT, ">>exceptions_added.txt");
						print FOUT $base, " ", $fix, "\n";
					
						print "Is there another combination?\n";
						chomp ($double = <>);
					
						if ($double eq 'y' or $double eq 'yes')
						{
							print "What is the base word?\n";
							chomp ($base = <>);
							print "What is the word to fix?\n";
							chomp ($fix = <>);
					
							$exceptions{$fix} = $base;
							print FOUT $base, " ", $fix, "\n";
						}
						close (FOUT);
					
					}
								
					$checked{$features[$k]}{$features[$e]} = 1;
					$checked{$features[$e]}{$features[$k]} = 1;
					
					open (FOUT, ">>checked.txt");
					print FOUT $features[$e], " ", $features[$k], "\n";
					print FOUT $features[$k], " ", $features[$e], "\n";
					close (FOUT);
				
				}
				
			}
			
		}
	}
}








###switch out the words
foreach $cue (sort keys %featurelist)
{
	foreach $target (sort keys %{$featurelist{$cue}})
	{
		
		print $cue, " ", $target, " \n";
		#####for words on the list
		if (exists $exceptions{$target})
		{
			if (exists $featurelist{$cue}{$exceptions{$target}})
			{
				print $exceptions{$target}, " original freq: $featurelist{$cue}{$target}, change freq: $featurelist{$cue}{$exceptions{$target}}\n";
				$featurelist_cor{$cue}{$exceptions{$target}} = $featurelist{$cue}{$target} + $featurelist{$cue}{$exceptions{$target}};
			}
			if (exists $featurelist_cor{$cue}{$exceptions{$target}})
			{
				$featurelist_cor{$cue}{$exceptions{$target}} = $featurelist{$cue}{$target} + $featurelist_cor{$cue}{$exceptions{$target}};
				print $exceptions{$target}, " original freq: $featurelist{$cue}{$target}, change freq: $featurelist_cor{$cue}{$exceptions{$target}}\n";
			}
			else
			{
				print "original freq: $featurelist{$cue}{$target}\n";
				$featurelist_cor{$cue}{$exceptions{$target}} = $featurelist{$cue}{$target};
			}
		}
		else
		{
			$featurelist_cor{$cue}{$target} = $featurelist{$cue}{$target};
		}
		
	}
}	

#####print the final list


open (FOUT, ">final_features.txt");

foreach $cue (sort keys %featurelist_cor)
{
	foreach $target (sort keys %{$featurelist_cor{$cue}})
	{
		
		print FOUT $cue, " ", $target, " ", $featurelist_cor{$cue}{$target}, "\n";
	}
}


close (FOUT);
			
			