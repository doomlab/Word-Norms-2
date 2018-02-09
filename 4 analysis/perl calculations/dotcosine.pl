open(FIN,"final words.txt");
while(<FIN>) {
next if /#/;
#next if /v_/;
#next if /m_/;

#where	cue	feature	translated	frequency_feature	frequency_translated	n	normalized_feature	normalized_translated	pos_cue	pos_feature	pos_translated	a1	a2	a3	FSG	BSG

chomp; tr/ \t/ /s;
($where, $cue, $feature, $translated, $freq_feature, $freq_trans, $totaln, $norm_feature, $norm_trans, $pos_cue, $pos_feature, $pos_trans, @affixes) = split();

##create combinations with features
$rawword{$where.$cue}{$feature} = $norm_feature;
$rootword{$where.$cue}{$translated} = $norm_trans;

#print "$where $cue $feature $norm_feature $norm_trans\n";


##affix loop
    for ($i = 0; $i < @affixes; $i++)
    {
        next if ($affixes[$i] eq 0);
	if (defined $affix{$where.$cue}{$translated}{$affixes[$i]})
        {
        $affixtot{$where.$cue}{$translated}{$affixes[$i]} = $affixtot{$where.$cue}{$translated}{$affixes[$i]} + 1;
	
	$affix{$where.$cue}{$translated}{$affixes[$i].$affixtot{$where.$cue}{$translated}{$affixes[$i]}} = $norm_feature;
	
        #print "$where $cue $translated $affixes[$i] $affixtot{$where}{$cue}{$translated}{$affixes[$i]}\n";
	#print "$affix{$where}{$cue}{$translated}{$affixes[$i].$affixtot{$where}{$cue}{$translated}{$affixes[$i]}}\n";
        } ##if affix defined
        
        else
        {
        $affix{$where.$cue}{$translated}{$affixes[$i]} = $norm_feature;
	$affixtot{$where.$cue}{$translated}{$affixes[$i]} = 1;
        #print "$where $cue $translated $affixes[$i] $affix{$where}{$cue}{$translated}{$affixes[$i]}\n";
        }
            
    }##for loop

}##while loop


##create feature frequencies

##raw words
   
	    foreach $word (sort keys %rawword)
	    {
		    foreach $feature (sort keys %{$rawword{$word}})
		    {
			    if (not defined $rawfrequency{$word}) {$rawfrequency{$word} = 0;}
			
			$rawfrequency{$word} = $rawfrequency{$word} + ($rawword{$word}{$feature}*$rawword{$word}{$feature});
			#print "$where $word $rawfrequency{$where}{$word} $rawword{$where}{$word}{$feature}\n";
		    }
	      }

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

##affixes
	foreach $word (sort keys %affix)
	{
		foreach $root (sort keys %{$affix{$word}})
		{
			foreach $affix (sort keys %{$affix{$word}{$root}})
			{
				if (not defined $affixfrequency{$word}) {$affixfrequency{$word} = 0;}
				$affixfrequency{$word} = $affixfrequency{$word} + ($affix{$word}{$root}{$affix}*$affix{$word}{$root}{$affix});
				#print "$where $word $affixfrequency{$where}{$word} $affix{$where}{$word}{$root}{$affix}\n";
			}
		}
	}

##root cosine values

	foreach $word1 (sort keys %rootword)
	{
		#print $word, " ";
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
				
				if (defined $totalmatchroot{$word1}{$word2}) {$totalmatchroot{$word1}{$word2} = $totalmatchroot{$word1}{$word2} + 1;}
				else {$totalmatchroot{$word1}{$word2}  = 1;}
				#print "$where $word1 $word2 $totalmatchroot{$where}{$word1}{$word2}\n";
			  
			    }##if matches
			
			}##foreach word2 target words
			
		    
		}##foreach word2
	    
	}##foreach word1


####this part matches the features and gets the total multiplication total 
####numerator

##affix cosine values
	foreach $word1 (sort keys %affix)
	{
		#print $word, " ";
		foreach $word2 (sort keys %affix)
		{
			#print "$word2\n";
			foreach $target (sort keys %{$affix{$word2}})
			{
			    #print "word 1 = $word1 word 2 = $word2 \n feature 2 = $target\n";

		    if (defined $affix{$word1}{$target})
		    {
				#print "matched target\n";
				##create all possible pairs for combining later
				if (not defined $totalaffix{$word1}{$word2}) {$totalaffix{$word1}{$word2} = 0;}
			    
				foreach $affix (sort keys %{$affix{$word2}{$target}})
				{
					    
				  if (defined $affix{$word1}{$target}{$affix})
				    {
				      $multiply = $affix{$word1}{$target}{$affix}*$affix{$word2}{$target}{$affix};
				      $totalaffix{$word1}{$word2} = $multiply + $totalaffix{$word1}{$word2};
				      #print "$word1 $word2 $target $affix $multiply\n";
				if (defined $totalmatchaffix{$word1}{$word2}) {$totalmatchaffix{$word1}{$word2} = $totalmatchaffix{$word1}{$word2} + 1;}
				else {$totalmatchaffix{$word1}{$word2}  = 1;}
			  
				}##if matches
				
			   }##foreach affix
			    
			    }##if defined feature stuff
			
			}##foreach word2 target words
			
		    
		}##foreach word2
	    
	}##foreach word1


##raw cosine values
	foreach $word1 (sort keys %rawword)
	{
		#print $word, " ";
		foreach $word2 (sort keys %rawword)
		{
			#print "$word2\n";
			foreach $target (sort keys %{$rawword{$word2}})
			{
			    #print "word 1 = $word1 word 2 = $word2 \n feature 2 = $target\n";

			    ##create all possible pairs for combining later
			    if (not defined $totalraw{$word1}{$word2}) {$totalraw{$word1}{$word2} = 0;}
			    
			    if (defined $rawword{$word1}{$target})
			    {
				$multiply = $rawword{$word1}{$target}*$rawword{$word2}{$target};
				$totalraw{$word1}{$word2} = $multiply + $totalraw{$word1}{$word2};
				if (defined $totalmatchraw{$word1}{$word2}) {$totalmatchraw{$word1}{$word2} = $totalmatchraw{$word1}{$word2} + 1;}
				else {$totalmatchraw{$word1}{$word2}  = 1;}
			  
			    }##if matches
			
			}##foreach word2 target words
			
		    
		}##foreach word2
	    
	}##foreach word1


open (FOUTRAW, ">all raw values.txt");
open (FOUTROOT, ">all root values.txt");
open (FOUTAFF, ">all affix values.txt");

print "Now printing\n";
####this section figures out the bottom of the dot cosine - the product of the total features to be able to divide by

	foreach $word1 (sort keys %totalroot)
	{
		foreach $word2 (sort keys %{$totalroot{$word1}})
		{
			#print $word1, " ", $word2, " ", $totalraw{$word1}{$word2}, " ", $rawfrequency{$word1}, " ", $rawfrequnecy{$word2}, "\n";
			
			if (defined $totalraw{$word1}{$word2}){
				if($totalraw{$word1}{$word2} != 0){
					if($totalmatchraw{$word1}{$word2} > 1) {
						$cosraw{$word1}{$word2} = $totalraw{$word1}{$word2} / (sqrt($rawfrequency{$word1}) * sqrt($rawfrequency{$word2}));
						
						if (not defined $defraw{$word1}{$word2} or not defined $defraw{$word2}{$word1})
						{
							$defraw{$word1}{$word2} = 1;
							$defraw{$word2}{$word1} = 1;
							print FOUTRAW $word1, " ", $word2, " ", $cosraw{$word1}{$word2}, " \n"; 
						}
					}
				}
			}
			
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
				
			if(defined $totalaffix{$word1}{$word2}){
				if($totalaffix{$word1}{$word2} != 0){
					if($totalmatchaffix{$word1}{$word2} > 1){
						$cosaffix{$word1}{$word2} = $totalaffix{$word1}{$word2} / (sqrt($affixfrequency{$word1}) * sqrt($affixfrequency{$word2}));
						
						if (not defined $defaff{$word1}{$word2} or not defined $defaff{$word2}{$word1})
						{
							$defaff{$word1}{$word2} = 1;
							$defaff{$word2}{$word1} = 1;
						print FOUTAFF $word1, " ", $word2, " ", $cosaffix{$word1}{$word2}, " \n"; 
						}
					}
				}
			}			
			
		}
	}

close (FOUTRAW);
close (FOUTROOT);
close (FOUTAFF);
