open (FIN, "all words.txt");

##cue	feature	frequency	sample size	normalized	translated

while (<FIN>){
	
	chomp;
	tr/ \t/ /s;
	tr/A-Z/a-z/;
	tr/,/ /s;

($target, $feature, $frequency, $n, $percent, $translate) = split;

print "$target, $feature, $frequency, $n, $percent, $translate\n"; 

next if ($target eq $translate);
next if ($target eq $feature);
next if ($feature eq 'use');
next if ($feature eq 'where');
next if ($feature eq 'have');
next if ($feature eq 'do');
next if ($feature eq 'them');
next if ($feature eq 'describe');
next if ($feature eq 'about');
next if ($feature eq 'but');
next if ($feature eq 'such');
next if ($feature eq 'lot');
next if ($feature eq 'their');
next if ($feature eq 'means');
next if ($feature eq 'could');
next if ($feature eq 'word');
next if ($feature eq 'may');
next if ($feature eq 'i');
next if ($feature eq 'any');
next if ($feature eq 'around');
next if ($feature eq 'there');
next if ($feature eq 'how');
next if ($feature eq 'its');
next if ($feature eq 'mean');
next if ($feature eq 'don');
next if ($feature eq 'doing');
next if ($feature eq 'typical');
next if ($feature eq 'typically');
next if ($feature eq 'my');
next if ($feature eq 'between');
next if ($feature eq 'we');
next if ($feature eq 'anything');
next if ($feature eq 'much');
next if ($feature eq 'so');
next if ($feature eq 'too');
next if ($feature eq 'lots');
next if ($feature eq 'describes');
next if ($feature eq 'was');
next if ($feature eq 'what');
next if ($feature eq 'your');
next if ($feature eq 'being');
next if ($feature eq 'very');
next if ($feature eq 'some');
next if ($feature eq 't');
next if ($feature eq 'who');
next if ($feature eq 'they');
next if ($feature eq 'no');
next if ($feature eq 'thing');
next if ($feature eq 'than');
next if ($feature eq 'another');
next if ($feature eq 'this');
next if ($feature eq 'into');
next if ($feature eq 'put');
next if ($feature eq 'have');
next if ($feature eq 'a');
next if ($feature eq 'of');
next if ($feature eq 'to');
next if ($feature eq 'in');
next if ($feature eq 'the');
next if ($feature eq 'the');
next if ($feature eq 'is');
next if ($feature eq 'or');
next if ($feature eq 'and');
next if ($feature eq 'be');
next if ($feature eq 'that');
next if ($feature eq 'an');
next if ($feature eq 'something');
next if ($feature eq 'for');
next if ($feature eq 'with');
next if ($feature eq 'on');
next if ($feature eq 'it');
next if ($feature eq 'not');
next if ($feature eq 'by');
next if ($feature eq 'you');
next if ($feature eq 'as');
next if ($feature eq 'from');
next if ($feature eq 'are');
next if ($feature eq 's');
next if ($feature eq 'other');
next if ($feature eq 'usually');
next if ($feature eq 'someone');
next if ($feature eq 'like');
next if ($feature eq 'out');
next if ($feature eq 'when');
next if ($feature eq 'things');
next if ($feature eq 'at');
next if ($feature eq 'get');
next if ($feature eq 'everyone');
next if ($feature eq 'looks');
next if ($feature eq 'our');
next if ($feature eq 'refer');
next if ($feature eq 'only');
next if ($feature eq 'either');
next if ($feature eq 'me');
next if ($feature eq 'us');
next if ($feature eq 'norm');
next if ($feature eq 'considered');
next if ($feature eq 'similar');
next if ($feature eq 'generally');
next if ($feature eq 'just');
next if ($feature eq 'particular');
next if ($feature eq 'ones');
next if ($feature eq 'doesn');
next if ($feature eq 'been');
next if ($feature eq 'based');
next if ($feature eq 'somewhere');
next if ($feature eq 'then');
next if ($feature eq 'while');
next if ($feature eq 'necessary');
next if ($feature eq 'various');
next if ($feature eq 'stuff');
next if ($feature eq 'requires');
next if ($feature eq 'would');
next if ($feature eq 'let');
next if ($feature eq 'upon');
next if ($feature eq 'R.txt');
next if ($feature eq 'non');
next if ($feature eq 'each');
next if ($feature eq 'every');
next if ($feature eq 'refers');
next if ($feature eq 'everything');
next if ($feature eq 'yourself');
next if ($feature eq 'should');
next if ($feature eq 'must');
next if ($feature eq 'taking');
next if ($feature eq 'become');
next if ($feature eq 'normally');
next if ($feature eq 'cannot');
next if ($feature eq 'keeps');
next if ($feature eq 'esp');
next if ($feature eq 'because');
next if ($feature eq 'never');
next if ($feature eq 'all');
next if ($feature eq 'am');
next if ($feature eq 'isn');
next if ($feature eq 'im');
next if ($feature eq 'have');
next if ($feature eq 'has');
next if ($feature eq 'had');
next if ($feature eq 'having');
next if ($feature eq 'made');
next if ($feature eq 'make');
next if ($feature eq 'makes');
next if ($feature eq 'making');
next if ($feature eq 'go');
next if ($feature eq 'goes');
next if ($feature eq 'gone');
next if ($feature eq 'going');
next if ($feature eq 'do');
next if ($feature eq 'does');
next if ($feature eq 'doing');
next if ($feature eq 'done');
next if ($feature eq 'can');
next if ($feature eq 'uses');
next if ($feature eq 'use');
next if ($feature eq 'using');
next if ($feature eq 'used');
next if ($feature eq 'cant');
next if ($feature eq 'else');
next if ($feature eq 'were');
next if ($feature eq 'whom');
next if ($feature eq 'hasn');
next if ($feature eq 'referring');
next if ($feature eq 'isnt');
next if ($feature eq 'ist');
next if ($feature eq 'it');
next if ($feature eq 'ive');
next if ($feature eq 'weren');
next if ($feature eq 'whom');

$code{$target}{$translate}{$feature} = $frequency;

if (not defined $freq{$target}{$translate})
    {
        $freq{$target}{$translate} = $frequency;
	}
else{
    $freq{$target}{$translate} = $freq{$target}{$translate} + $frequency;

    }

$people{$target}{$translate} = $n;

#$original{$target}{$feature} = $feature;

}

foreach $target (sort keys %freq)
{
    foreach $translate (sort keys %{$freq{$target}})
    {  
        $percent{$target}{$translate} = $freq{$target}{$translate}/$people{$target}{$translate} * 100;
	#print "$target $translate $people{$target}{$translate} $freq{$target}{$translate}\n";
        }
        }

foreach $target (sort keys %percent)
{
	$sort = 0;
    foreach $translate (sort { $percent{$target}{$b} <=> $percent{$target}{$a} or $a cmp $b } keys %{$percent{$target}})
    {  
	$sortorder{$target}{$translate} = $sort + 1; 
	#print "$target $translate $percent{$target}{$translate} $sortorder{$target}{$translate}\n";
	$sort++;
    }
}

foreach $target (sort keys %percent)
{
	foreach $translate (sort keys %{$percent{$target}})
	{
		##if percent > 16 or if 1-5
		if ($percent{$target}{$translate} >=16 || $sortorder{$target}{$translate} < 6)
		{
			#print "$target $translate $translate $freq{$target}{$translate} $freq{$target}{$translate} $people{$target}{$translate} $percent{$target}{$translate} $sortorder{$target}{$translate}\n";
			foreach $feature (sort keys %{$code{$target}{$translate}})
			{
			print "$target $feature $translate $code{$target}{$translate}{$feature} $freq{$target}{$translate} $people{$target}{$translate} $percent{$target}{$translate} $sortorder{$target}{$translate}\n";
			}
		}
	}
}






###C:\Users\ausername\Desktop\norming work\raw frequency files\1 to do