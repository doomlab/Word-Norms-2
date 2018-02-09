spp_master = read.csv("spp_analyze_file.csv") 

##do some subsetting magic 
##subsetting by type
spp_first = subset(spp_master, type == "first_assoc")
spp_other = subset(spp_master, type == "other_assoc")

##seperating out each dv
ldt200rt_1 = spp_first[ , c(4:9, 12, 22:26, 53:54, 57, 65)] ##Dropping all columns but the ones we need

ldt200rt_2 = spp_other[ , c(4:9, 12, 22:26, 53:54, 57, 65)] ##no catagoricals now

ldt200z_1 = spp_first[ , c(4:9, 12, 22:25, 27, 53:54, 57, 65)] ##still need nelson

ldt200z_2 = spp_other[ , c(4:9, 12, 22:25, 27, 53:54, 57, 65)]

ldt1200rt_1 = spp_first[ , c(4:9, 12, 22:25, 29, 53:54, 57, 65)]

ldt1200rt_2 = spp_other[, c(4:9, 12, 22:25, 29, 53:54, 57, 65)]

ldt1200z_1 = spp_first[, c(4:9, 12, 22:25, 30, 53:54, 57, 65)]

ldt1200z_2 = spp_other[, c(4:9, 12, 22:25, 30, 53:54, 55, 65)]

##once all 8 subsets are made, just change the dataset on the right of the equals sign 
condition = ldt200rt_1 ##then change dv name in each model.

##datascreening
##accuracy
summary(condition)
##finish this part based on whats needed

output = lm(LDT.200ms.RT ~ PLength + PSubFreq + POrthoN + TLength + TSubFreq + TOrthoN +
              FAS + BAS + PCueFanOut + TTargetFanIn +
              jcn + root + raw + affix +
              LSA , data = condition) 
output

##outliers
##mahal
mahal = mahalanobis(condition, 
                       colMeans(condition, na.rm = TRUE),   ##Figured out that the na's were fucking this up
                       cov(condition, use="pairwise.complete.obs")) ##also running as pairwise makes this part work
cutoff = qchisq(1-.001, ncol(condition))
cutoff
ncol(condtition[ , -1]) 
badmahal = as.numeric(mahal > cutoff) 
table(badmahal)

##leverage
k = 15 ##this number will change as we add iv's 
leverage = hatvalues(output)
cutleverage = (2*k+2) / nrow(condition) 
cutleverage 
badleverage = as.numeric(leverage > cutleverage)
table(badleverage)

##cooks
cooks = cooks.distance(output)
cutcooks = 4 / (nrow(condition) - k - 1) 
cutcooks 
badcooks = as.numeric(cooks > cutcooks)
table(badcooks)

##overall
totalout = badmahal + badleverage + badcooks
table(totalout)
spp_noout = subset(condition, totalout < 2)

new_output = lm(LDT.200ms.RT ~ PLength + PSubFreq + POrthoN + TLength + TSubFreq + TOrthoN +
              FAS + BAS + PCueFanOut + TTargetFanIn +
              jcn + root + raw + affix +
              LSA, data = spp_noout) 
new_output

##additivity
correl = cor(spp_noout, use="pairwise.complete.obs") 
correl
symnum(correl)

##assumptions
standardized = rstudent(output)
fitted = scale(output$fitted.values)
hist(standardized)
qqnorm(standardized)
abline(0,1)
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

library(QuantPsyc)
library(ppcor)

##first step (word info)
model1 = lm(LDT.200ms.RT ~ PLength + PSubFreq + POrthoN + TLength + TSubFreq + TOrthoN, data = spp_noout) 
summary(model1)
lm.beta(model1)
partials = pcor(spp_noout, method = "pearson") ##check numbers in brackets
partials$estimate ^ 2 

##second step (association)
model2 = lm(LDT.200ms.RT ~ PLength + PSubFreq + POrthoN + TLength + TSubFreq + TOrthoN +
              FAS + BAS + PCueFanOut + TTargetFanIn, data = spp_noout) 
summary(model2)
lm.beta(model2)
partials = pcor(spp_noout, method = "pearson") ##check numbers in brackets
partials$estimate ^ 2

##third step (semantics)
model3 = lm(LDT.200ms.RT ~ PLength + PSubFreq + POrthoN + TLength + TSubFreq + TOrthoN +
              FAS + BAS + PCueFanOut + TTargetFanIn +
              jcn + root + raw + affix, data = spp_noout) ##need to add in nelson stuff
summary(model3)
lm.beta(model3)
partials = pcor(spp_noout, method = "pearson") ##check numbers in brackets
partials$estimate ^ 2

##fourth step (thematics)
model4 = lm(LDT.200ms.RT ~ PLength + PSubFreq + POrthoN + TLength + TSubFreq + TOrthoN +
              FAS + BAS + PCueFanOut + TTargetFanIn +
              jcn + root + raw + affix +
              LSA, data = spp_noout) ##need to add in cosine/mandera stuff
summary(model4) 
lm.beta(model4)
partials = pcor(spp_noout, method = "pearson") ##check numbers in brackets
partials$estimate ^ 2

##comparing models
anova(model1, model2) 
anova(model1, model3)
anova(model1, model4)

anova(model2, model3)
anova(model2, model4)

anova(model3, model4)

##four steps total
##also graph(s)?
