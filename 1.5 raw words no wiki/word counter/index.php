<?php
/*
//
// ---Word Counter---
//
// By: Scott Buchanan (McNeese State, Lake Charles)
// Date: 11-23-2010
//
// Contact: inthebrillantblue@yahoo.com
//
*/

//Sets working directory
$directory = "./files/";
//Finds all .txt files in working directory
$files = glob("" . $directory . "*.txt");
 
//Process each .txt file
foreach($files as $file){
	
	//Check to see if .txt file exists
	if (file_exists($file)) {
		//Print file name for record
		echo "Now processing file ".$file."...<br />";
		//Get contents of the .txt file
		$lines = file_get_contents($file);
		//Removes certain chars, makes it easier to get words
		$chars = array(",", ".", "\n", "\w", "\t", "!", "#", "%", "^", "&", "*", "(", ")", "_", "-", "+", "=", "\\", "|", "]", "}", "[", "{", "'", "\"", "/", "?", ">", "<", "`", "~", ";", ":", "'", "\"", "");
		$lines = str_replace($chars, " ", $lines);
		//Lowercase all the letters
		$lines = strtolower($lines);
		//Break apart words into array
		$line = explode(' ', $lines);
		//Gets rid of empty elements in array
		$line = array_filter($line);
		
		//Holds the words and their counts
		$appears_array = array();
		$count_array = array();
		
		//Count words
		foreach($line as $word){
			//If word is already in array, add one to its count
			if(in_array($word, $appears_array)){
				$count_array[$word] = $count_array[$word]+1;
			}else{//If not, add the word with a count of 1
				$appears_array[] = $word;
				$count_array[$word] = 1;	
			}
		}
		
		sort($appears_array);
		
		//Open file
		$fp = fopen("./results/".str_replace("./files/", "", $file), 'a+');
		//Write beginning line
		//fwrite($fp, "Results for file ".$file."\n");
		
		//Display results
		echo "<p />Results:<br />";
		foreach($appears_array as $word){
			echo $word." ".$count_array[$word]."<br />";
			//And write to file
			fwrite($fp, $word." ".$count_array[$word]."\n");
		}
		echo "<p />";
		
		//Close file handler
		fclose($fp);

	}else{//File doesnt exist for some strange reason, after it was found by glob();
		echo "File doesnt exist.";	
	}
}

?>