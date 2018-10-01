open(FIN,"mv_reprocess.txt");
while(<FIN>) {
next if /#/;
#next if /v_/;
#next if /m_/;

#cue target features where

chomp; tr/ \t/ /s;
($cue,$target,$features,$where) = split(",");

$where = substr($where,0,1);

##create combinations with features
$rootword{$where.$cue}{$target} = $features;

#print "$where $cue $target $features\n";
}

#create feature frequencies

##root words
	foreach $word (sort keys %rootword)
	    {
		    foreach $feature (sort keys %{$rootword{$word}})
		    {
			    if (not defined $rootfrequency{$word}) {$rootfrequency{$word} = 0;}
			
			$rootfrequency{$word} = $rootfrequency{$word} + ($rootword{$word}{$feature}*$rootword{$word}{$feature});
			#print "$where $word $rootfrequency{$where}{$word} $rootword{$where}{$word}{$feature}\n";
		    }
	    }

##root cosine values

	foreach $word1 (sort keys %rootword)
	{
		print $word1, " \n";
		foreach $word2 (sort keys %rootword)
		{
			#print "$word2\n";
			foreach $target (sort keys %{$rootword{$word2}})
			{
			    #print "word 1 = $word1 word 2 = $word2 \n feature 2 = $target\n";

			    ##create all possible pairs for combining later
			    if (not defined $totalroot{$word1}{$word2}) {$totalroot{$word1}{$word2} = 0;}
			    
			    if (defined $rootword{$word1}{$target})
			    {
				$multiply = $rootword{$word1}{$target}*$rootword{$word2}{$target};
			        $totalroot{$word1}{$word2} = $multiply + $totalroot{$word1}{$word2};
                }
				
				if (defined $totalmatchroot{$word1}{$word2}) {$totalmatchroot{$word1}{$word2} = $totalmatchroot{$word1}{$word2} + 1;}
				
                else {$totalmatchroot{$word1}{$word2}  = 1;}
				
                #print "$where $word1 $word2 $totalmatchroot{$where}{$word1}{$word2}\n";
			  
			    }##if matches
			
			}##foreach word2 target words
	    
	}##foreach word1


####this part matches the features and gets the total multiplication total 
####numerator

open (FOUTROOT, ">all mv root values.txt");

print "Now printing\n";
####this section figures out the bottom of the dot cosine - the product of the total features to be able to divide by

	foreach $word1 (sort keys %totalroot)
	{
		foreach $word2 (sort keys %{$totalroot{$word1}})
		{
			#print $word1, " ", $word2, " ", $totalraw{$word1}{$word2}, " ", $rawfrequency{$word1}, " ", $rawfrequnecy{$word2}, "\n";
			
		
			if(defined $totalroot{$word1}{$word2}){
				if($totalroot{$word1}{$word2} != 0){
					if($totalmatchroot{$word1}{$word2} > 1) {
						$cosroot{$word1}{$word2} = $totalroot{$word1}{$word2} / (sqrt($rootfrequency{$word1}) * sqrt($rootfrequency{$word2}));
						
						if (not defined $defroot{$word1}{$word2} or not defined $defroot{$word2}{$word1})
						{
							$defroot{$word1}{$word2} = 1;
							$defroot{$word2}{$word1} = 1;
						print FOUTROOT $word1, " ", $word2, " ", $cosroot{$word1}{$word2}, " \n"; 
						}
					}
				}
			}
        }
    }

close (FOUTROOT);
