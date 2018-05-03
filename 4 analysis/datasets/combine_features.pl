open(FIN,"final words.txt");
while(<FIN>) {
next if /#/;
#next if /v_/;
#next if /m_/;

##where	cue	feature	translated	frequency_feature	frequency_translated	n	normalized_feature	normalized_translated	pos_cue	pos_feature	pos_translated	a	a	a

chomp; tr/ \t/ /s;
($where, $cue, $feature, $translated,
 $frequency_feature, $frequency_translated, $n,
 $normalized_feature, $normalized_translated, $pos_cue, $pos_feature, $pos_translated, $a1, $a2, $a3) = split();

 $n{$where}{$cue}{$feature}{$translated} = $n;
 $pos_cue{$where}{$cue}{$feature}{$translated} = $pos_cue;
 $pos_feature{$where}{$cue}{$feature}{$translated} = $pos_feature;
 $pos_trans{$where}{$cue}{$feature}{$translated} = $pos_translated;
 $a1{$where}{$cue}{$feature}{$translated} = $a1;
 $a2{$where}{$cue}{$feature}{$translated} = $a2;
 $a3{$where}{$cue}{$feature}{$translated} = $a3;
 
 
##figure out if something is already defined
if (defined $freq_feat{$where}{$cue}{$feature}{$translated}){
    
    ##if v or m
    if ($where eq 'v' or $where eq 'm') {
        ##select largest
        #print $freq_feat{$where}{$cue}{$feature}{$translated}, " ", $frequency_feature, "\n";
        if ($freq_feat{$where}{$cue}{$feature}{$translated} < $frequency_feature)
        {
            #print $freq_feat{$where}{$cue}{$feature}{$translated}, " ", $frequency_feature, "\n"; 
            $freq_feat{$where}{$cue}{$feature}{$translated} = $frequency_feature;
        }
        
        
    }##end if m or v
    else {
        $freq_feat{$where}{$cue}{$feature}{$translated} = $freq_feat{$where}{$cue}{$feature}{$translated} + $frequency_feature;
    } ##end if else
} ##end if defined

else {
    $freq_feat{$where}{$cue}{$feature}{$translated} = $frequency_feature;
    
} ### end else

}##while loop

close(FIN);

##calculate frequency translated
foreach $where (sort keys %freq_feat) {
    foreach $cue (sort keys %{$freq_feat{$where}}) {
        foreach $feature (sort keys %{$freq_feat{$where}{$cue}}){
            foreach $translated (sort keys %{$freq_feat{$where}{$cue}{$feature}})
            {
                if (defined $freq_trans{$where}{$cue}{$translated}) {
                   #print $freq_trans{$where}{$cue}{$translated}, "\n";
                   $freq_trans{$where}{$cue}{$translated} = $freq_trans{$where}{$cue}{$translated} + $freq_feat{$where}{$cue}{$feature}{$translated}; 
                }
                else {
                    $freq_trans{$where}{$cue}{$translated} = $freq_feat{$where}{$cue}{$feature}{$translated};
                    #print $freq_trans{$cue}{$translated}, "\n";
                }
                
            }
        }
    }
}

foreach $where (sort keys %freq_feat) {
    foreach $cue (sort keys %{$freq_feat{$where}}) {
        foreach $feature (sort keys %{$freq_feat{$where}{$cue}}){
            foreach $translated (sort keys %{$freq_feat{$where}{$cue}{$feature}})
            {
                print "$where $cue $feature $translated $freq_feat{$where}{$cue}{$feature}{$translated} ";
                print "$freq_trans{$where}{$cue}{$translated} $n{$where}{$cue}{$feature}{$translated} $pos_cue{$where}{$cue}{$feature}{$translated} ";
                print "$pos_feature{$where}{$cue}{$feature}{$translated} $pos_trans{$where}{$cue}{$feature}{$translated} ";
                print "$a1{$where}{$cue}{$feature}{$translated} $a2{$where}{$cue}{$feature}{$translated} $a3{$where}{$cue}{$feature}{$translated}\n";               
            }
        }
    }
}