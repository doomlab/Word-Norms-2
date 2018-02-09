#Load Libraries
library("tm")
library("SnowballC")
library("lsa")
library("ggplot2")

#Set Pathname
pathname = "C:/Users/Caleb/Desktop/Fall 2016/Lab Work/LSA Work Folder"

# read files into a document-term matrix
myMatrix = textmatrix(pathname, 
                      minWordLength = 1,
                      stopwords = stopwords_en)
table(myMatrix)
myMatrix = lw_logtf(myMatrix) * gw_idf(myMatrix)

# create the latent semantic space
myLSAspace = lsa(myMatrix, dims=dimcalc_share())

# display it as a textmatrix again for viewing
checkvalues = table(round(as.textmatrix(myLSAspace),2)) # should give the original

#Convert LSA to Text-Frequency Vector Matrix for Cosines
myNewMatrix = as.textmatrix(myLSAspace)

#NewMatrixtable = table(myNewMatrix)
#View(NewMatrixtable)

# compare two terms with the cosine measure
#Working towards terms to use here:
#cosine(myNewMatrix["lord",], myNewMatrix["god",])
cosinematrix = cosine(myNewMatrix)
View(cosinematrix)

# compare two documents with pearson
cor(myNewMatrix[,1], myNewMatrix[,2], method="pearson")

#save correlation as variable
matrixcorrelation = 
  cor(myNewMatrix[,1], myNewMatrix[,2], method="pearson")

#Create CSV for 1_39 <=> 56_66
write.csv(cosinematrix, "SubtlEX_LSA_Cosines.csv")

#Hypothesis Test Information
matrixcorrelation

