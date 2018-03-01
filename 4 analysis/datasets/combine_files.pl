open(FIN,"all raw values.txt");
while(<FIN>) {
chomp; tr/ \t/ /s;
($combocue, $combotarget, $cos) = split();

$wherec = substr($combocue, 0, 1);
$cue = substr($combocue, 1);
$wheret = substr($combotarget, 0, 1);
$target = substr($combotarget, 1);

#print $wherec, " ", $cue, " ", $wheret, " ", $target, "\n";

if(defined $raw{$cue}{$target}) { $raw{$cue}{$target} = ($raw{$cue}{$target} + $cos)/2;}
else {$raw{$cue}{$target} = $cos;}
$rawwhere{$wherec.".".$cue}{$wheret.".".$target} = $cos;

}
close(FIN);

open(FIN,"all root values.txt");
while(<FIN>) {
chomp; tr/ \t/ /s;
($combocue, $combotarget, $cos) = split();

$wherec = substr($combocue, 0, 1);
$cue = substr($combocue, 1);
$wheret = substr($combotarget, 0, 1);
$target = substr($combotarget, 1);

if(defined $root{$cue}{$target}) { $root{$cue}{$target} = ($root{$cue}{$target} + $cos)/2;}
else {$root{$cue}{$target} = $cos;}

$rootwhere{$wherec.".".$cue}{$wheret.".".$target} = $cos;

}
close(FIN);

open(FIN,"all affix values.txt");
while(<FIN>) {
chomp; tr/ \t/ /s;

($combocue, $combotarget, $cos) = split();

$wherec = substr($combocue, 0, 1);
$cue = substr($combocue, 1);
$wheret = substr($combotarget, 0, 1);
$target = substr($combotarget, 1);

if(defined $affix{$cue}{$target}) { $affix{$cue}{$target} = ($affix{$cue}{$target} + $cos)/2;}
else {$affix{$cue}{$target} = $cos;}

$affixwhere{$wherec.".".$cue}{$wheret.".".$target} = $cos;

}
close(FIN);

open(FIN,"usfjcnlsa.txt");
while(<FIN>) {
chomp; tr/ \t/ /s;
#cue	target	fsg	bsg	msg	qcon	tcon	qfr	tfr	jcn	lsa
($cue, $target, $fsg, $bsg, $msg, $qcon, $tcon, $qfr, $tfr, $jcn, $lsa) = split();

$jcn{$cue}{$target} = $jcn;
$lsa{$cue}{$target} = $lsa;

#print "$cue $target $jcn $lsa\n";

}
close(FIN); 

open(FIN,"usf_norms.txt");
while(<FIN>) {
chomp; tr/ \t/ /s;
#CUE	TARGET	 FSG	 BSG	 MSG	 OSG	 QSS	 QFR	 QCON	 QPS	 TSS	 TFR	 TCON	 TPS
($cue, $target, $fsg, $bsg, @dump) = split();

$fsg{$cue}{$target} = $fsg;
$bsg{$cue}{$target} = $bsg;

#print "$cue $target $fsg $bsg\n";

}
close(FIN);

open(FIN,"oldcos.txt");
while(<FIN>) {
chomp; tr/ \t/ /s;

($cue, $target, $cos) = split();


$oldcos{$cue}{$target} = $cos;

#print "$cue $target $cos\n";

}
close(FIN);



open(FOUT, ">all where cosine.txt");
foreach $cue (sort keys %rootwhere)
{
    foreach $target (sort keys %{$rootwhere{$cue}})
    {
        next if (defined $done{$cue}{$target});
        
        print FOUT "$cue $target $rootwhere{$cue}{$target} ";
        if (defined $rawwhere{$cue}{$target}){ print FOUT "$rawwhere{$cue}{$target} ";}
        else { print FOUT "0 ";}
        
        if (defined $affixwhere{$cue}{$target}){ print FOUT "$affixwhere{$cue}{$target} ";}
        else { print FOUT "0 ";}

        $cue1 = substr($cue, 2);
        $target1 = substr($target, 2);
        
        #print $cue1, " ", $target1, "\n";
        
        if (defined $oldcos{$cue1}{$target1}){ print FOUT "$oldcos{$cue1}{$target1} ";}
        else { print FOUT "NULL ";}
        
        if (defined $jcn{$cue1}{$target1}){ print FOUT "$jcn{$cue1}{$target1} "; }
        else { print FOUT "NULL "; }

        if (defined $lsa{$cue1}{$target1}){ print FOUT "$lsa{$cue1}{$target1} "; }
        else { print FOUT "NULL "; }        

        if (defined $fsg{$cue1}{$target1}){ print FOUT "$fsg{$cue1}{$target1} "; }
        else { print FOUT "NULL "; }

        if (defined $bsg{$cue1}{$target1}){ print FOUT "$bsg{$cue1}{$target1} \n"; }
        else { print FOUT "NULL \n"; }
        
        $done{$cue}{$target} = 1;
        $done{$target}{$cue} = 1;        
    }
}
        
close(FOUT);        

open(FOUT, ">all averaged cosine.txt");
foreach $cue (sort keys %root)
{
    foreach $target (sort keys %{$root{$cue}})
    {
        next if (defined $done{$cue}{$target});
        
        print FOUT "$cue $target $root{$cue}{$target} ";
        if (defined $raw{$cue}{$target}){ print FOUT "$raw{$cue}{$target} ";}
        else { print FOUT "0 ";}
        
        if (defined $affix{$cue}{$target}){ print FOUT "$affix{$cue}{$target} ";}
        else { print FOUT "0 ";}
  
        if (defined $oldcos{$cue}{$target}){ print FOUT "$oldcos{$cue}{$target} ";}
        else { print FOUT "NULL ";}
        
        if (defined $jcn{$cue}{$target}){ print FOUT "$jcn{$cue}{$target} "; }
        else { print FOUT "NULL "; }

        if (defined $lsa{$cue}{$target}){ print FOUT "$lsa{$cue}{$target} "; }
        else { print FOUT "NULL "; }        

        if (defined $fsg{$cue}{$target}){ print FOUT "$fsg{$cue}{$target} "; }
        else { print FOUT "NULL "; }

        if (defined $bsg{$cue}{$target}){ print FOUT "$bsg{$cue}{$target} \n"; }
        else { print FOUT "NULL \n"; }
        
        $done{$cue}{$target} = 1;
        $done{$target}{$cue} = 1;        
    }
}
        
close(FOUT);        



##ended up doing this manually in R
#open(FOUT, ">spp_data.txt");

##put this at the end so you can dump things into it
#open(FIN,"items_spp2.txt");
#while(<FIN>) {


#chomp; tr/ \t/ /s;

#type	prime_firstassociate	TARGET	f_subtlfreq	f_logsubtlfreq	f_length	f_LogHal	f_orthoN	f_freq_greater	f_BG_SUM	f_BG_mean	f_POS	f_ldt_RT_ELP	f_ldt_Z_ELP	f_ldt_acc_ELP	f_name_RT_ELP	f_name_Z_ELP	f_name_acc_ELP	t_length	t_LogHal	t_subtlfreq	t_logsubtlfreq	t_orthoN	t_freq_greater	t_BG_SUM	t_BG_mean	t_POS	t_ldt_RT	t_ldt_Z	t_ldt_acc	t_name_RT	t_name_Z	t_name_acc	relation1_f-t	relation2_f-t	FAS_f-t	BAS_f-t	f_CUEfanout	TARGETfanin	rank	LSA_f-t	BEAGLE_RPs	BEAGLE_PMI	BEAGLEcosine	BEAGLEWIKININ	BEAGLEWIKICosine

#($type, $cue, $target, @dump) = split();
    
   
#    print FOUT "$type $cue $target ";
    
#    if (defined $jcn{$cue}{$target}){ print FOUT "$jcn{$cue}{$target} "; }
#    else { print FOUT "NULL "; }
    
#    if (defined $root{$cue}{$target}){ print FOUT "$root{$cue}{$target} ";}
#    else { print FOUT "0 ";}
    
#    if (defined $raw{$cue}{$target}){ print FOUT "$raw{$cue}{$target} ";}
#    else { print FOUT "0 ";}
        
#    if (defined $affix{$cue}{$target}){ print FOUT "$affix{$cue}{$target} ";}
#    else { print FOUT "0 ";}
    
#    print FOUT "@dump\n";
    
  
#}
#close(FIN);
#close(FOUT);
