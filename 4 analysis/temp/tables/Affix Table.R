##This script created for the DOOM Lab SPP Project
setwd("C:/Users/Caleb/Desktop/Lab Work/R Source")
library(readxl)
library(reshape)
library(memisc)
final_words_2017 <- read_excel
View(final_words_2017)

##Creating Affix dataset
affixdata = final_words_2017[,13:15]
View(affixdata)

##Melting dataset
affixdata$wordno = 1:nrow(affixdata)
longdata = melt(affixdata,
                id = "wordno",
                measured = c("a1", "a2", "a3"))
View(longdata)
colnames(longdata) = c("partno", "order", "affix")

summary(longdata)

##Removing zero affixes##
longaffix = longdata
longaffix$affix[longaffix$affix == 0] = NA
summary(longaffix)
realaffix = na.omit(longaffix)
summary(realaffix)
View(realaffix)

##Actual percentages for real
final = realaffix
options(scipen = 999)
affixtable = percent(droplevels(final$affix))
affixtable

slangwords = subset(final_words_2017, a1 == "slang")
slangwords$feature
