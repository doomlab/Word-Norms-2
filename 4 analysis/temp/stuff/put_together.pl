#~ ####our cosine values
for ($r = 1; $r<21; $r++)
{
open(FIN,"all cosine values long".$r.".txt");
while(<FIN>) {
next if /#/;
chomp; tr/ \t/ /s;
($worda, $wordb, $cosine) = split();

$word1 = lc($worda);
$word2 = lc($wordb);

$w1 = substr($word1, 2);
$w2 = substr($word2, 2);

if (not defined $cos{$w1}{$w2})
{
	$cos{$w1}{$w2} = $cosine;
}
else
{
	$cos{$w1}{$w2} = $cos{$w1}{$w2} + $cosine;
	
	if (not defined $totals{$w1}{$w2})
	{
		$totals{$w1}{$w2} = 1;
	}
	
	$totals{$w1}{$w2} = $totals{$w1}{$w2} + 1;
}
}

close(FIN);
}

foreach $w1 (sort keys %totals)
{
	foreach $w2 (sort keys %{$totals{$w1}})
	{
		$cos{$w1}{$w2} = $cos{$w1}{$w2} / $totals{$w1}{$w2};
		
		#print "$w1 $w2 $cos{$w1}{$w2} $totals{$w1}{$w2} \n";
	}
}

#~ ########

#~ ####nelson norms

open(FIN,"usf_norms.txt");
while(<FIN>) {
next if /^CUE TARGET/;

chomp; 
tr/ \t/ /s;

($cue2, $target2, $fsg, $bsg, $msg, $osg, $qss, $qfr, $qcon, $qps, $tss, $tfr, $tcon, $tps) = split();

$cue=lc($cue2);
$target=lc($target2);

$fsg{$cue}{$target} = $fsg;
$bsg{$cue}{$target} = $bsg;

###singles
$qss{$cue} = $qss;
$KF{$cue} = $qfr;
$qcon{$cue} = $qcon;
$POS{$cue} = $qps;
$tss{$target} = $tss; 
$KF{$target} = $tfr;
$tcon{$target} = $tcon;
$POS{$target} = $tps;


#$qps2{lc($cue)}= $qps;
#$tps2{lc($target)}= $tps;

#print $cue, " ", $target, " ", $fsg{$cue}{$target}, "\n";

}
close (FIN);


#####ELP norms
##Word	Length	Freq_KF	Freq_HAL	Log_Freq_HAL	SUBTLWF	LgSUBTLWF	SUBTLCD	LgSUBTLCD	Ortho_N	
##Phono_N	OG_N	Freq_N	Freq_N_P	Freq_N_OG	NPhon	NSyll	NMorph	POS

open(FIN,"elp.txt");
while(<FIN>) {

chomp; tr/ \t/ /s;

($word2, $length, $kf, $hal, $loghal, $subl, $subllog, $subt, $subtlog, $ortho, $phono, $ogn, $freqN, $freqNP, $freqNO, $phonemes, $syllables, $morphemes, $pos) = split(" ");

$word=lc($word2);

$length{$word} = $length;
$KF{$word} = $kf;
$HAL{$word} = $hal;
$LOGHAL{$word} = $loghal;
$subtlex{$word} = $subl;
$subtlexlog{$word} = $subllog;
$ortho{$word} = $ortho;
$phono{$word} = $phono;
$phonemes{$word} = $phonemes;
$syllables{$word} = $syllables;
$morphemes{$word} = $morphemes;
$POS{$word} = $pos;

#print $word, $KF{$word}, $POS{$word}, "\n";

}
close (FIN);

####this section was for part of speech
#~ open(FIN,"all words and features.txt");
#~ while(<FIN>) {

#~ chomp; tr/ \t/ /s;

#~ $word = lc($_);

#~ $tagme{$word} = 1;

#~ }
#~ close(FIN);

#~ foreach $word (sort keys %tagme)
#~ {
	#~ if (not defined $qps2{$word})
	#~ {
		#~ $qps2{$word} = "null";
	#~ }
	
	#~ if (not defined $tps2{$word})
	#~ {
		#~ $tps2{$word} = "null";
	#~ }
	
	#~ if (not defined $pos{$word})
	#~ {
		#~ $pos{$word} = "null";
	#~ }

	
	#~ print $word, " ", $qps2{$word}, " ", $tps2{$word}, " ", $pos{$word}, " \n";
#~ }

#~ ########maki norms
open(FIN,"usfjcnlsa.txt");
while(<FIN>) {
next if /#/;
next if /cue,target,/;
chomp; tr/ \t/ /s;

($cue2, $target2, $fsg, $bsg, $msg, $qcon,$tcon, $qfr, $tfr, $jcn, $lsa) = split(" ");

$cue=lc($cue2);
$target = lc($target2);

$jcn{$cue}{$target} = $jcn;
$lsa{$cue}{$target} = $lsa;
$jcn{$target}{$cue} = $jcn;
$lsa{$target}{$cue} = $lsa;


#print $cue, " ", $target, " ", $jcn{$cue}{$target}, "\n";

##cue,target,fsg,bsg,msg,qcon,tcon,qfr,tfr,jcn,lsa
}
close(FIN);

####print that shit out.

open (FOUT, ">>complete database.txt");

foreach $w1 (sort keys %cos)
{
	foreach $w2 (sort keys %{$cos{$w1}})
	{

		###if cos is greater than .05
		if ($cos{$w1}{$w2} > 0.04999)
		{
			
		####paste all the stuff
		if (not defined $jcn{$w1}{$w2})
		{
			$jcn{$w1}{$w2} = "null";
		}
		
		if (not defined $lsa{$w1}{$w2})
		{
			$lsa{$w1}{$w2} = "null";
		}
		
		if (not defined $bsg{$w1}{$w2})
		{
			$bsg{$w1}{$w2} = "null";
		}
		
		if (not defined $fsg{$w1}{$w2})
		{
			$fsg{$w1}{$w2} = "null";
		}

		if (not defined $qss{$w1})
		{
			$qss{$w1} = "null";
		}
		
		if (not defined $tss{$w2})
		{
			$tss{$w2} = "null";
		}
		
		if (not defined $tcon{$w2})
		{
			$tcon{$w2} = "null";
		}

		if (not defined $qcon{$w1})
		{
			$qcon{$w1} = "null";
		}

		if (not defined $KF{$w1})
		{
			$KF{$w1} = "null";
		}

		if (not defined $KF{$w2})
		{
			$KF{$w2} = "null";
		}

		if (not defined $HAL{$w1})
		{
			$HAL{$w1} = "null";
		}

		if (not defined $HAL{$w2})
		{
			$HAL{$w2} = "null";
		}
		
		if (not defined $LOGHAL{$w1})
		{
			$LOGHAL{$w1} = "null";
		}

		if (not defined $LOGHAL{$w2})
		{
			$LOGHAL{$w2} = "null";
		}

		if (not defined $subtlex{$w1})
		{
			$subtlex{$w1} = "null";
		}

		if (not defined $subtlex{$w2})
		{
			$subtlex{$w2} = "null";
		}
		
		if (not defined $subtlexlog{$w1})
		{
			$subtlexlog{$w1} = "null";
		}

		if (not defined $subtlexlog{$w2})
		{
			$subtlexlog{$w2} = "null";
		}
		
		if (not defined $length{$w1})
		{
			$length{$w1} = length($w1);
		}

		if (not defined $length{$w2})
		{
			$length{$w2} = length($w2);
		}

		if (not defined $POS{$w1})
		{
			$POS{$w1} = "null";
		}

		if (not defined $POS{$w2})
		{
			$POS{$w2} = "null";
		}
		
		if (not defined $ortho{$w1})
		{
			$ortho{$w1} = "null";
		}

		if (not defined $ortho{$w2})
		{
			$ortho{$w2} = "null";
		}

		if (not defined $phono{$w1})
		{
			$phono{$w1} = "null";
		}

		if (not defined $phono{$w2})
		{
			$phono{$w2} = "null";
		}

		if (not defined $phonemes{$w1})
		{
			$phonemes{$w1} = "null";
		}

		if (not defined $phonemes{$w2})
		{
			$phonemes{$w2} = "null";
		}
		
		if (not defined $syllables{$w1})
		{
			$syllables{$w1} = "null";
		}

		if (not defined $syllables{$w2})
		{
			$syllables{$w2} = "null";
		}
		
		if (not defined $morphemes{$w1})
		{
			$morphemes{$w1} = "null";
		}

		if (not defined $morphemes{$w2})
		{
			$morphemes{$w2} = "null";
		}
				
	
				
		print FOUT "$w1 $w2 $cos{$w1}{$w2} $jcn{$w1}{$w2} $lsa{$w1}{$w2} $fsg{$w1}{$w2} $bsg{$w1}{$w2} $qss{$w1}";
		print FOUT " $tss{$w2} $qcon{$w1} $tcon{$w2} $KF{$w1} $KF{$w2} $HAL{$w1} $HAL{$w2} $LOGHAL{$w1} $LOGHAL{$w2} ";
		print FOUT "$subtlex{$w1} $subtlex{$w2} $subtlexlog{$w1} $subtlexlog{$w2} $length{$w1} $length{$w2} $POS{$w1} $POS{$w2} ";
		print FOUT "$ortho{$w1} $ortho{$w2} $phono{$w1} $phono{$w2} $phonemes{$w1} $phonemes{$w2} $syllables{$w1} $syllables{$w2} ";
		print FOUT "$morphemes{$w1} $morphemes{$w2}\n";
	}
	
	elsif (defined $fsg{$w1}{$w2})
	{
	
		if (not defined $jcn{$w1}{$w2})
		{
			$jcn{$w1}{$w2} = "null";
		}
		
		if (not defined $lsa{$w1}{$w2})
		{
			$lsa{$w1}{$w2} = "null";
		}
		
		if (not defined $bsg{$w1}{$w2})
		{
			$bsg{$w1}{$w2} = "null";
		}
		
		if (not defined $fsg{$w1}{$w2})
		{
			$fsg{$w1}{$w2} = "null";
		}

		if (not defined $qss{$w1})
		{
			$qss{$w1} = "null";
		}
		
		if (not defined $tss{$w2})
		{
			$tss{$w2} = "null";
		}
		
		if (not defined $tcon{$w2})
		{
			$tcon{$w2} = "null";
		}

		if (not defined $qcon{$w1})
		{
			$qcon{$w1} = "null";
		}

		if (not defined $KF{$w1})
		{
			$KF{$w1} = "null";
		}

		if (not defined $KF{$w2})
		{
			$KF{$w2} = "null";
		}

		if (not defined $HAL{$w1})
		{
			$HAL{$w1} = "null";
		}

		if (not defined $HAL{$w2})
		{
			$HAL{$w2} = "null";
		}
		
		if (not defined $LOGHAL{$w1})
		{
			$LOGHAL{$w1} = "null";
		}

		if (not defined $LOGHAL{$w2})
		{
			$LOGHAL{$w2} = "null";
		}

		if (not defined $subtlex{$w1})
		{
			$subtlex{$w1} = "null";
		}

		if (not defined $subtlex{$w2})
		{
			$subtlex{$w2} = "null";
		}
		
		if (not defined $subtlexlog{$w1})
		{
			$subtlexlog{$w1} = "null";
		}

		if (not defined $subtlexlog{$w2})
		{
			$subtlexlog{$w2} = "null";
		}
		
		if (not defined $length{$w1})
		{
			$length{$w1} = length($w1);
		}

		if (not defined $length{$w2})
		{
			$length{$w2} = length($w2);
		}

		if (not defined $POS{$w1})
		{
			$POS{$w1} = "null";
		}

		if (not defined $POS{$w2})
		{
			$POS{$w2} = "null";
		}
		
		if (not defined $ortho{$w1})
		{
			$ortho{$w1} = "null";
		}

		if (not defined $ortho{$w2})
		{
			$ortho{$w2} = "null";
		}

		if (not defined $phono{$w1})
		{
			$phono{$w1} = "null";
		}

		if (not defined $phono{$w2})
		{
			$phono{$w2} = "null";
		}

		if (not defined $phonemes{$w1})
		{
			$phonemes{$w1} = "null";
		}

		if (not defined $phonemes{$w2})
		{
			$phonemes{$w2} = "null";
		}
		
		if (not defined $syllables{$w1})
		{
			$syllables{$w1} = "null";
		}

		if (not defined $syllables{$w2})
		{
			$syllables{$w2} = "null";
		}
		
		if (not defined $morphemes{$w1})
		{
			$morphemes{$w1} = "null";
		}

		if (not defined $morphemes{$w2})
		{
			$morphemes{$w2} = "null";
		}
				
	
				
		print FOUT "$w1 $w2 $cos{$w1}{$w2} $jcn{$w1}{$w2} $lsa{$w1}{$w2} $fsg{$w1}{$w2} $bsg{$w1}{$w2} $qss{$w1}";
		print FOUT " $tss{$w2} $qcon{$w1} $tcon{$w2} $KF{$w1} $KF{$w2} $HAL{$w1} $HAL{$w2} $LOGHAL{$w1} $LOGHAL{$w2} ";
		print FOUT "$subtlex{$w1} $subtlex{$w2} $subtlexlog{$w1} $subtlexlog{$w2} $length{$w1} $length{$w2} $POS{$w1} $POS{$w2} ";
		print FOUT "$ortho{$w1} $ortho{$w2} $phono{$w1} $phono{$w2} $phonemes{$w1} $phonemes{$w2} $syllables{$w1} $syllables{$w2} ";
		print FOUT "$morphemes{$w1} $morphemes{$w2}\n";
		
	}
	

	}
}

close(FOUT);