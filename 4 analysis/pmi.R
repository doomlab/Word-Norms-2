##set working directory to datasets
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Word-Norms-2/4 analysis/datasets")

##pull in the cosine information
alldata = read.table("all averaged cosine.txt", quote = "\"", comment.char = "")
colnames(alldata) = c("cue", "target", "root", "raw", "affix", "old", "jcn", "lsa", "fsg", "bsg")

#rot cosine only
rootcos = alldata[, c("cue", "target", "root")]
#fix into wide dataset
wideroot = dcast(rootcos, cue ~ target)
rownames(wideroot) = wideroot$cue #set up row names
wideroot = wideroot[ , -1] #drop cue so it's cues by target only
wideroot[is.na(wideroot)] = 0 #fix all connections to zero
wideroot = as.table(as.matrix(wideroot)) #coerce into a table

##try pmi
library(svs)
pmi_matrix = pmi(wideroot, base = 2, normalize = T)
write.csv(pmi_matrix, "pmi_matrix.csv")


##try random walk
#identity matrix minus alpha times inverse pmi
alpha = .25
rw_matrix = diag(nrow(pmi_matrix)) - alpha * solve(pmi_matrix)