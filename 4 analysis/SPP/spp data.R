####let's make a dataset####
##set the working directory to the datasets folder
library(readxl)
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Word-Norms-2/4 analysis/datasets")

##import all the files
##spp specific priming
firstassocLDT = read.csv("assoc_relatedLDT.csv")
firstassocN = read.csv("assoc_relatedN.csv")
otherassocLDT = read.csv("other_assoc_relatedLDT.csv")
otherassocN = read.csv("other_assoc_relatedN.csv")
##other databases to join
cosine_data = read.table("all averaged cosine.txt", quote= "\"", comment.char = "")
colnames(cosine_data) = c("first", "second", "root", "raw", "affix", "old", "jcn", "lsa", "fsg", "bsg")
cbow = read.csv("cbow mandera.csv")
elp = read.csv("elp.csv")
targets = read.csv("targetwords.csv")
usf = read_excel("usf_norms.xlsx")
maki = read.csv("usfjcnlsa.csv")

####clean up the cue target columns for everything####

##lower case
firstassocLDT$TargetWord = tolower(as.character(firstassocLDT$TargetWord))
firstassocLDT$Prime = tolower(as.character(firstassocLDT$Prime))
firstassocN$TargetWord = tolower(as.character(firstassocN$TargetWord))
firstassocN$Prime = tolower(as.character(firstassocN$Prime))
otherassocLDT$TargetWord = tolower(as.character(otherassocLDT$TargetWord))
otherassocLDT$Prime = tolower(as.character(otherassocLDT$Prime))
otherassocN$TargetWord = tolower(as.character(otherassocN$TargetWord))
otherassocN$Prime = tolower(as.character(otherassocN$Prime))
cosine_data$first = tolower(as.character(cosine_data$first))
cosine_data$second = tolower(as.character(cosine_data$second))
cbow$word_1 = tolower(as.character(cbow$word_1))
cbow$word_2 =  tolower(as.character(cbow$word_2))
elp$Word =  tolower(as.character(elp$Word))
targets$TargetWord = tolower(as.character(targets$TargetWord))
usf$CUE = tolower(as.character(usf$CUE))
usf$TARGET = tolower(as.character(usf$TARGET))
maki$cue = tolower(as.character(maki$cue))
maki$target = tolower(as.character(maki$target))

##take out ' symbols
firstassocLDT$TargetWord = gsub("'", "", firstassocLDT$TargetWord)
firstassocLDT$Prime = gsub("'", "", firstassocLDT$Prime)
firstassocN$TargetWord = gsub("'", "", firstassocN$TargetWord)
firstassocN$Prime = gsub("'", "", firstassocN$Prime)
otherassocLDT$TargetWord = gsub("'", "", otherassocLDT$TargetWord)
otherassocLDT$Prime = gsub("'", "", otherassocLDT$Prime)
otherassocN$TargetWord = gsub("'", "", otherassocN$TargetWord)
otherassocN$Prime = gsub("'", "", otherassocN$Prime)
cosine_data$first = gsub("'", "", cosine_data$first)
cosine_data$second = gsub("'", "", cosine_data$second)
cbow$word_1 = gsub("'", "", cbow$word_1)
cbow$word_2 =  gsub("'", "", cbow$word_2)
elp$Word =  gsub("'", "", elp$Word)
targets$TargetWord = gsub("'", "", targets$TargetWord)
usf$CUE = gsub("'", "", usf$CUE)
usf$TARGET = gsub("'", "", usf$TARGET)
maki$cue = gsub("'", "", maki$cue)
maki$target = gsub("'", "", maki$target)

##take out spaces
firstassocLDT$TargetWord = gsub(" ", "", firstassocLDT$TargetWord)
firstassocLDT$Prime = gsub(" ", "", firstassocLDT$Prime)
firstassocN$TargetWord = gsub(" ", "", firstassocN$TargetWord)
firstassocN$Prime = gsub(" ", "", firstassocN$Prime)
otherassocLDT$TargetWord = gsub(" ", "", otherassocLDT$TargetWord)
otherassocLDT$Prime = gsub(" ", "", otherassocLDT$Prime)
otherassocN$TargetWord = gsub(" ", "", otherassocN$TargetWord)
otherassocN$Prime = gsub(" ", "", otherassocN$Prime)
cosine_data$first = gsub(" ", "", cosine_data$first)
cosine_data$second = gsub(" ", "", cosine_data$second)
cbow$word_1 = gsub(" ", "", cbow$word_1)
cbow$word_2 =  gsub(" ", "", cbow$word_2)
elp$Word =  gsub(" ", "", elp$Word)
targets$TargetWord = gsub(" ", "", targets$TargetWord)
usf$CUE = gsub(" ", "", usf$CUE)
usf$TARGET = gsub(" ", "", usf$TARGET)
maki$cue = gsub(" ", "", maki$cue)
maki$target = gsub(" ", "", maki$target)

##make index column
firstassocLDT$index = paste(firstassocLDT$Prime,firstassocLDT$TargetWord, sep = ".")
firstassocN$index = paste(firstassocN$Prime, firstassocN$TargetWord, sep = ".")
otherassocLDT$index = paste(otherassocLDT$Prime,otherassocLDT$TargetWord, sep = ".")
otherassocN$index = paste(otherassocN$Prime, otherassocN$TargetWord, sep = ".")
cosine_data$index = paste(cosine_data$first, cosine_data$second, sep = ".")
cbow$index = paste(cbow$word_1, cbow$word_2, sep = ".")
usf$index = paste(usf$CUE, usf$TARGET, sep = ".")
maki$index = paste(maki$cue, maki$target, sep = ".")

##deal with the forward backward problem, cosine, jcn
cosine_data2 = cosine_data
cosine_data2$index = paste(cosine_data2$second, cosine_data2$first, sep = ".")
cosine_combined = rbind(cosine_data, cosine_data2)
cosine_combined = unique(cosine_combined) #get rid of word-word
maki2  = maki
maki2$index = paste(maki2$target, maki2$cue, sep = ".")
maki_combined = rbind(maki, maki2)
maki_combined = maki_combined[ !duplicated(maki_combined$index), ]
cbow2  = cbow
cbow2$index = paste(cbow2$word_2, cbow2$word_1, sep = ".")
cbow_combined = rbind(cbow, cbow2)
cbow_combined = cbow_combined[ !duplicated(cbow_combined$index), ]

#convert column names for file since they are the prime word information
colnames(firstassocLDT)[3:14] = paste("P", colnames(firstassocLDT)[3:14], sep = "")
colnames(firstassocN)[3:14] = paste("P", colnames(firstassocN)[3:14], sep = "")
colnames(otherassocLDT)[3:14] = paste("P", colnames(otherassocLDT)[3:14], sep = "")
colnames(otherassocN)[3:14] = paste("P", colnames(otherassocN)[3:14], sep = "")
colnames(targets)[2:13] = paste("T", colnames(targets)[2:13], sep = "")

##add single word information
FALDT = merge(firstassocLDT, targets, by = "TargetWord", all.x = T)
FAN = merge(firstassocN, targets, by = "TargetWord", all.x = T)
OALDT = merge(otherassocLDT, targets, by = "TargetWord", all.x = T)
OAN = merge(otherassocN, targets, by = "TargetWord", all.x = T)

##add in paired information
#cosine
FALDT = merge(FALDT, cosine_combined, by = "index", all.x = T)
FAN = merge(FAN, cosine_combined, by = "index", all.x = T)
OALDT = merge(OALDT, cosine_combined, by = "index", all.x = T)
OAN = merge(OAN, cosine_combined, by = "index", all.x = T)

#fix the zeros
FALDT$root[is.na(FALDT$root)] = 0
FAN$root[is.na(FAN$root)] = 0
OALDT$root[is.na(OALDT$root)] = 0
OAN$root[is.na(OAN$root)] = 0

FALDT$raw[is.na(FALDT$raw)] = 0
FAN$raw[is.na(FAN$raw)] = 0
OALDT$raw[is.na(OALDT$raw)] = 0
OAN$raw[is.na(OAN$raw)] = 0

FALDT$affix[is.na(FALDT$affix)] = 0
FAN$affix[is.na(FAN$affix)] = 0
OALDT$affix[is.na(OALDT$affix)] = 0
OAN$affix[is.na(OAN$affix)] = 0

##cbow
FALDT = merge(FALDT, cbow_combined, by = "index", all.x = T)
FAN = merge(FAN, cbow_combined, by = "index", all.x = T)
OALDT = merge(OALDT, cbow_combined, by = "index", all.x = T)
OAN = merge(OAN, cbow_combined, by = "index", all.x = T)

##usf
FALDT = merge(FALDT, usf, by = "index", all.x = T)
FAN = merge(FAN, usf, by = "index", all.x = T)
OALDT = merge(OALDT, usf, by = "index", all.x = T)
OAN = merge(OAN, usf, by = "index", all.x = T)

##maki
FALDT = merge(FALDT, maki_combined, by = "index", all.x = T)
FAN = merge(FAN, maki_combined, by = "index", all.x = T)
OALDT = merge(OALDT, maki_combined, by = "index", all.x = T)
OAN = merge(OAN, maki_combined, by = "index", all.x = T)

write.csv(FAN, "FAN.csv", row.names = F)
write.csv(FALDT, "FALDT.csv", row.names = F)
write.csv(OALDT, "OALDT.csv", row.names = F)
write.csv(OAN, "OAN.csv", row.names = F)

# ###ditched the elp since the data is not one to one
# colnames(elp)[1] = "TargetWord" 
# FALDT = merge(FALDT, elp, by = "TargetWord", all.x = T)
# FAN = merge(FAN, elp, by = "TargetWord", all.x = T)
# OALDT = merge(OALDT, elp, by = "TargetWord", all.x = T)
# OAN = merge(OAN, elp, by = "TargetWord", all.x = T)
# colnames(FALDT)[47:64] = paste("ET", colnames(FALDT)[47:64], sep = "")
# colnames(FAN)[47:64] = paste("ET", colnames(FAN)[47:64], sep = "")
# colnames(OALDT)[47:64] = paste("ET", colnames(OALDT)[47:64], sep = "")
# colnames(OAN)[47:64] = paste("ET", colnames(OAN)[47:64], sep = "")
# 
# colnames(elp)[1] = "Prime" 
# FALDT = merge(FALDT, elp, by = "Prime", all.x = T)
# FAN = merge(FAN, elp, by = "Prime", all.x = T)
# OALDT = merge(OALDT, elp, by = "Prime", all.x = T)
# OAN = merge(OAN, elp, by = "Prime", all.x = T)
# colnames(FALDT)[65:82] = paste("EP", colnames(FALDT)[65:82], sep = "")
# colnames(FAN)[65:82] = paste("EP", colnames(FAN)[65:82], sep = "")
# colnames(OALDT)[65:82] = paste("EP", colnames(OALDT)[65:82], sep = "")
# colnames(OAN)[65:82] = paste("EP", colnames(OAN)[65:82], sep = "")
