####let's make a dataset####
##set the working directory to the datasets folder
library(readxl)
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Word-Norms-2/4 analysis/datasets")

##import all the files
cosine_data = read.table("all averaged cosine.txt", quote= "\"", comment.char = "")
colnames(cosine_data) = c("first", "second", "root", "raw", "affix", "old", "jcn", "lsa", "fsg", "bsg")

firstassocLDT = read.csv("assoc_relatedLDT.csv")
firstassocN = read.csv("assoc_relatedN.csv")
cbow = read.csv("cbow mandera.csv")
elp = read.csv("elp.csv")
otherassocLDT = read.csv("other_assoc_relatedLDT.csv")
otherassocN = read.csv("other_assoc_relatedN.csv")
targets = read.csv("targetwords.csv")
usf = read_excel("usf_norms.xlsx")
maki = read.csv("usfjcnlsa.csv")

####keep each of the datasets by analysis separate
firstassocLDT$TargetWord = tolower(as.character(firstassocLDT$TargetWord))
firstassocLDT$Prime = tolower(as.character(firstassocLDT$Prime))

#convert column names for file since they are the prime word information
colnames(firstassocLDT)[3:14] = paste("P", colnames(firstassocLDT)[3:14], sep = "")

##take out the quotes
firstassocLDT$TargetWord = gsub("'", "", firstassocLDT$TargetWord)
firstassocLDT$Prime = gsub("'", "", firstassocLDT$Prime)

##make index column
firstassocLDT$index = paste(firstassocLDT$TargetWord, firstassocLDT$Prime, sep = "")

#add cosine
cosine_data$index = paste(cosine_data$first, cosine_data$second, sep = "")
FALDTtemp = merge(firstassocLDT, cosine_data, by = "index", all.x = T)

#add cbow
cbow$index = paste(cbow$word_1, cbow$word_2, sep = "")
FALDTtemp = merge(FALDTtemp, cbow, by = "index", all.x = T)

#add usf
usf$CUE = tolower(usf$CUE)
usf$TARGET = tolower(usf$TARGET)
usf$CUE = gsub("'", "", usf$CUE)
usf$TARGET = gsub("'", "", usf$TARGET)
usf$index = paste(usf$CUE, usf$TARGET, sep = "")
FALDTtemp = merge(FALDTtemp, usf, by = "index", all.x = T)

#add maki
maki$index = paste(maki$cue, maki$target, sep = "")
FALDTtemp = merge(FALDTtemp, maki, by = "index", all.x = T)


elp = read.csv("elp.csv")
targets = read.csv("targetwords.csv")
