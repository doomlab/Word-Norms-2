####setup####
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Word-Norms-2/4 analysis/datasets")

##load data sets
faldt.dat = read.csv("FALDT.csv")
fan.dat = read.csv("FAN.csv")
oaldt.dat = read.csv("OALDT.csv")
oan.dat = read.csv("OAN.csv")

options(scipen = 999)

##selecting just the columns needed for the analyses and removing missing data
faldt.nomiss = na.omit(faldt.dat[ , c(4:5, 8, 12, 18:35, 38, 42,
                                      46, 49:51, 59, 83, 86:92)])
summary(faldt.nomiss)

fan.nomiss = na.omit(fan.dat[ , c(4:5, 8, 12, 18:35, 38, 42,
                                  46, 49:51, 59, 83, 86:92)])
summary(fan.nomiss)

oaldt.nomiss = na.omit(oaldt.dat[ , c(4:5, 8, 12, 18:35, 38, 42,
                                      46, 49:51, 59, 83, 86:92)])
summary(oaldt.nomiss)                       

oan.nomiss = na.omit(oan.dat[ , c(4:5, 8, 12, 18:35, 38, 42,
                                  46, 49:51, 59, 83, 86:92)])
summary(oan.nomiss)

##selecting most common part of speech
##most common POS
faldt.nomiss$TPOS = substr(faldt.nomiss$TPOS, 0, 2)
faldt.nomiss$PPOS = substr(faldt.nomiss$PPOS, 0, 2)

faldt.nomiss$TPOS = gsub("mi|RB", "JJ", faldt.nomiss$TPOS)
faldt.nomiss$PPOS = gsub("mi|RB", "JJ", faldt.nomiss$PPOS)

fan.nomiss$TPOS = substr(fan.nomiss$TPOS, 0, 2)
fan.nomiss$PPOS = substr(fan.nomiss$PPOS, 0, 2)

fan.nomiss$TPOS = gsub("mi|RB", "JJ", fan.nomiss$TPOS)
fan.nomiss$PPOS = gsub("mi|RB", "JJ", fan.nomiss$PPOS)

oaldt.nomiss$TPOS = substr(oaldt.nomiss$TPOS, 0, 2)
oaldt.nomiss$PPOS = substr(oaldt.nomiss$PPOS, 0, 2)

oaldt.nomiss$TPOS = gsub("mi|RB", "JJ", oaldt.nomiss$TPOS)
oaldt.nomiss$PPOS = gsub("mi|RB", "JJ", oaldt.nomiss$PPOS)

oan.nomiss$TPOS = substr(oan.nomiss$TPOS, 0, 2)
oan.nomiss$PPOS = substr(oan.nomiss$PPOS, 0, 2)

oan.nomiss$TPOS = gsub("mi|RB", "JJ", oan.nomiss$TPOS)
oan.nomiss$PPOS = gsub("mi|RB", "JJ", oan.nomiss$PPOS)

##just do priming 200 and 1200
library(MASS)
library(relaimpo)

####Faldt####
##faldt 200RT priming

#word characteristics
faldt.200priming.1 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                   data = faldt.nomiss) 
stepAIC(faldt.200priming.1, direction = "both")$anova
#TLength

#associations
faldt.200priming.2 = lm(LDT.200ms.RT.Priming ~ 
                     BAS + FAS + CueFanOut + 
                     TargetFanIn,
                   data = faldt.nomiss) 
stepAIC(faldt.200priming.2, direction = "both")$anova
#BAS + TargetFanIn

# semantics
faldt.200priming.3 = lm(LDT.200ms.RT.Priming ~ PCosine_Set + 
                     TCosine_Set + 
                     Proot_feat_set + 
                     Troot_feat_set +
                     jcn.y + root + 
                     affix + distance,
                   data = faldt.nomiss)
stepAIC(faldt.200priming.3, direction = "both")$anova
#root + distance

#thematics
faldt.200priming.4 = lm(LDT.200ms.RT.Priming ~ 
                     LSA, 
                   data = faldt.nomiss)
stepAIC(faldt.200priming.4, direction = "both")$anova
#LSA

faldt.200priming.overall = lm(LDT.200ms.RT.Priming ~ 
                                TLength + BAS + TargetFanIn + 
                                root + distance + LSA,
                              data = faldt.nomiss)
summary(faldt.200priming.overall, correlation = T)

#relative importance 
calc.relimp(faldt.200priming.overall, 
            type = c("lmg", "last", "first", "betasq"), 
            rela = TRUE)

boot.faldt.200prime = boot.relimp(faldt.200priming.overall, 
                                  b = 1000, 
                                  type = c("lmg", "last", "first", "betasq"),
                                  rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot.faldt.200prime) # print result
plot(booteval.relimp(boot.faldt.200prime,sort=TRUE)) # plot result 
#sometimes the plot doesn't run unless you make it a really big plot window

##faldt 1200 priming
##word characteristics
faldt.1200priming.1 = lm(LDT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                        data = faldt.nomiss) 
stepAIC(faldt.1200priming.1, direction = "both")$anova
#Tlength

#associations
faldt.1200priming.2 = lm(LDT.1200ms.RT.Priming ~ 
                          BAS + FAS + CueFanOut + 
                          TargetFanIn,
                        data = faldt.nomiss) 
stepAIC(faldt.1200priming.2, direction = "both")$anova
##targetfanin + BAS

#semantics
faldt.1200priming.3 = lm(LDT.1200ms.RT.Priming ~ PCosine_Set + 
                          TCosine_Set + 
                          Proot_feat_set + 
                          Troot_feat_set +
                          jcn.y + root + 
                          affix + distance,
                        data = faldt.nomiss)
stepAIC(faldt.1200priming.3, direction = "both")$anova
##affix + distance + prootfeatset

#thematics
faldt.1200priming.4 = lm(LDT.1200ms.RT.Priming ~ 
                          LSA, 
                        data = faldt.nomiss)
stepAIC(faldt.1200priming.4, direction = "both")$anova
##LSA

faldt.1200priming.overall = lm(LDT.1200ms.RT.Priming ~ 
                                TLength + BAS + TargetFanIn + 
                                affix + distance + Proot_feat_set + LSA,
                              data = faldt.nomiss)
summary(faldt.1200priming.overall, correlation = T)

#relative importance 
calc.relimp(faldt.1200priming.overall, 
            type = c("lmg", "last", "first", "betasq"), 
            rela = TRUE)

boot.faldt.1200prime = boot.relimp(faldt.1200priming.overall, 
                                  b = 1000, 
                                  type = c("lmg", "last", "first", "betasq"),
                                  rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot.faldt.1200prime) # print result
plot(booteval.relimp(boot.faldt.1200prime,sort=TRUE))

####Fan####
##fan 200RT priming
##word characteristics
fan.200priming.1 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                         data = fan.nomiss) 
stepAIC(fan.200priming.1, direction = "both")$anova
#Plength

#associations
fan.200priming.2 = lm(NT.200ms.RT.Priming ~ 
                           BAS + FAS + CueFanOut + 
                           TargetFanIn,
                         data = fan.nomiss) 
stepAIC(fan.200priming.2, direction = "both")$anova
##targetfanin + BAS + CueFanOut

# semantics
fan.200priming.3 = lm(NT.200ms.RT.Priming ~ PCosine_Set + 
                           TCosine_Set + 
                           Proot_feat_set + 
                           Troot_feat_set +
                           jcn.y + root + 
                           affix + distance,
                         data = fan.nomiss)
stepAIC(fan.200priming.3, direction = "both")$anova
##TCosine_Set + PCosine_Set

#thematics
fan.200priming.4 = lm(NT.200ms.RT.Priming ~ 
                           LSA, 
                         data = fan.nomiss)
stepAIC(fan.200priming.4, direction = "both")$anova
##nothing

fan.200priming.overall = lm(NT.200ms.RT.Priming ~ 
                                 PLength + BAS + TargetFanIn + CueFanOut +
                              TCosine_Set + PCosine_Set,
                               data = fan.nomiss)
summary(fan.200priming.overall, correlation = T)

#relative importance 
calc.relimp(fan.200priming.overall, 
            type = c("lmg", "last", "first", "betasq"), 
            rela = TRUE)

boot.fan.200prime = boot.relimp(fan.200priming.overall, 
                                   b = 1000, 
                                   type = c("lmg", "last", "first", "betasq"),
                                   rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot.fan.200prime) # print result
plot(booteval.relimp(boot.fan.200prime,sort=TRUE))

##fan 1200RT priming
##word characteristics
fan.1200priming.1 = lm(NT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                      data = fan.nomiss) 
stepAIC(fan.1200priming.1, direction = "both")$anova
#TPOS + TOrthoN + TSubFreq

#associations
fan.1200priming.2 = lm(NT.1200ms.RT.Priming ~ 
                        BAS + FAS + CueFanOut + 
                        TargetFanIn,
                      data = fan.nomiss) 
stepAIC(fan.1200priming.2, direction = "both")$anova
##targetfanin + FAS

#semantics
fan.1200priming.3 = lm(NT.1200ms.RT.Priming ~ PCosine_Set + 
                        TCosine_Set + 
                        Proot_feat_set + 
                        Troot_feat_set +
                        jcn.y + root + 
                        affix + distance,
                      data = fan.nomiss)
stepAIC(fan.1200priming.3, direction = "both")$anova
##root + distance + Troot_feat_set

#thematics
fan.1200priming.4 = lm(NT.1200ms.RT.Priming ~ 
                        LSA, 
                      data = fan.nomiss)
stepAIC(fan.1200priming.4, direction = "both")$anova
##nothing

fan.1200priming.overall = lm(NT.1200ms.RT.Priming ~  ##took out POS since it screws up the models below
                               TOrthoN + TSubFreq +
                               TargetFanIn + FAS + 
                               root + distance + Troot_feat_set,
                            data = fan.nomiss)
summary(fan.1200priming.overall, correlation = T)

#relative importance 
calc.relimp(fan.1200priming.overall, ##gives a warning message.
            type = c("lmg", "last", "first", "betasq"), 
            rela = TRUE)

boot.fan.1200prime = boot.relimp(fan.1200priming.overall, 
                                b = 1000, 
                                type = c("lmg", "last", "first", "betasq"),
                                rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot.fan.1200prime) # print result
plot(booteval.relimp(boot.fan.1200prime,sort=TRUE))


####oaldt#####
#word characteristics
oaldt.200priming.1 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                        data = oaldt.nomiss) 
stepAIC(oaldt.200priming.1, direction = "both")$anova
#PLength + TPOS

#associations
oaldt.200priming.2 = lm(LDT.200ms.RT.Priming ~ 
                          BAS + FAS + CueFanOut + 
                          TargetFanIn,
                        data = oaldt.nomiss) 
stepAIC(oaldt.200priming.2, direction = "both")$anova
#FAS + TargetFanIn

# semantics
oaldt.200priming.3 = lm(LDT.200ms.RT.Priming ~ PCosine_Set + 
                          TCosine_Set + 
                          Proot_feat_set + 
                          Troot_feat_set +
                          jcn.y + root + 
                          affix + distance,
                        data = oaldt.nomiss)
stepAIC(oaldt.200priming.3, direction = "both")$anova
PCosine_Set + TCosine_Set + distance

#thematics
oaldt.200priming.4 = lm(LDT.200ms.RT.Priming ~ 
                          LSA, 
                        data = oaldt.nomiss)
stepAIC(oaldt.200priming.4, direction = "both")$anova
#LSA

oaldt.200priming.overall = lm(LDT.200ms.RT.Priming ~ 
                                PLength  + #taking out TPOS
                                FAS + TargetFanIn +
                                PCosine_Set + TCosine_Set + distance + LSA,
                              data = oaldt.nomiss)
summary(oaldt.200priming.overall, correlation = T)

#relative importance 
calc.relimp(oaldt.200priming.overall, 
            type = c("lmg", "last", "first", "betasq"), 
            rela = TRUE)

boot.oaldt.200prime = boot.relimp(oaldt.200priming.overall, 
                                  b = 1000, 
                                  type = c("lmg", "last", "first", "betasq"),
                                  rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot.oaldt.200prime) # print result
plot(booteval.relimp(boot.oaldt.200prime,sort=TRUE)) # plot result 
#sometimes the plot doesn't run unless you make it a really big plot window

##oaldt 1200 priming
##word characteristics
oaldt.1200priming.1 = lm(LDT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                         data = oaldt.nomiss) 
stepAIC(oaldt.1200priming.1, direction = "both")$anova
#Plength

#associations
oaldt.1200priming.2 = lm(LDT.1200ms.RT.Priming ~ 
                           BAS + FAS + CueFanOut + 
                           TargetFanIn,
                         data = oaldt.nomiss) 
stepAIC(oaldt.1200priming.2, direction = "both")$anova
##nothing

#semantics
oaldt.1200priming.3 = lm(LDT.1200ms.RT.Priming ~ PCosine_Set + 
                           TCosine_Set + 
                           Proot_feat_set + 
                           Troot_feat_set +
                           jcn.y + root + 
                           affix + distance,
                         data = oaldt.nomiss)
stepAIC(oaldt.1200priming.3, direction = "both")$anova
##TCosine_Set + distance

#thematics
oaldt.1200priming.4 = lm(LDT.1200ms.RT.Priming ~ 
                           LSA, 
                         data = oaldt.nomiss)
stepAIC(oaldt.1200priming.4, direction = "both")$anova
##LSA

oaldt.1200priming.overall = lm(LDT.1200ms.RT.Priming ~ 
                                 PLength + 
                                 TCosine_Set + distance + LSA,
                               data = oaldt.nomiss)
summary(oaldt.1200priming.overall, correlation = T)

#relative importance 
calc.relimp(oaldt.1200priming.overall, 
            type = c("lmg", "last", "first", "betasq"), 
            rela = TRUE)

boot.oaldt.1200prime = boot.relimp(oaldt.1200priming.overall, 
                                   b = 1000, 
                                   type = c("lmg", "last", "first", "betasq"),
                                   rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot.oaldt.1200prime) # print result
plot(booteval.relimp(boot.oaldt.1200prime,sort=TRUE))

####oan####
##word characteristics
oan.200priming.1 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                      data = oan.nomiss) 
stepAIC(oan.200priming.1, direction = "both")$anova
#PLength + TSubFreq

#associations
oan.200priming.2 = lm(NT.200ms.RT.Priming ~ 
                        BAS + FAS + CueFanOut + 
                        TargetFanIn,
                      data = oan.nomiss) 
stepAIC(oan.200priming.2, direction = "both")$anova
##BAS + CueFanOut

#semantics
oan.200priming.3 = lm(NT.200ms.RT.Priming ~ PCosine_Set + 
                        TCosine_Set + 
                        Proot_feat_set + 
                        Troot_feat_set +
                        jcn.y + root + 
                        affix + distance,
                      data = oan.nomiss)
stepAIC(oan.200priming.3, direction = "both")$anova
##PCosine_Set + Proot_feat_set

#thematics
oan.200priming.4 = lm(NT.200ms.RT.Priming ~ 
                        LSA, 
                      data = oan.nomiss)
stepAIC(oan.200priming.4, direction = "both")$anova
##nothing

oan.200priming.overall = lm(NT.200ms.RT.Priming ~ 
                              PLength + TSubFreq + BAS + CueFanOut +
                              PCosine_Set + Proot_feat_set,
                            data = oan.nomiss)
summary(oan.200priming.overall, correlation = T)

#relative importance 
calc.relimp(oan.200priming.overall, 
            type = c("lmg", "last", "first", "betasq"), 
            rela = TRUE)

boot.oan.200prime = boot.relimp(oan.200priming.overall, 
                                b = 1000, 
                                type = c("lmg", "last", "first", "betasq"),
                                rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot.oan.200prime) # print result
plot(booteval.relimp(boot.oan.200prime,sort=TRUE))

##oan 1200RT priming
##word characteristics
oan.1200priming.1 = lm(NT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                       data = oan.nomiss) 
stepAIC(oan.1200priming.1, direction = "both")$anova
#TLength + TOrthoN

#associations
oan.1200priming.2 = lm(NT.1200ms.RT.Priming ~ 
                         BAS + FAS + CueFanOut + 
                         TargetFanIn,
                       data = oan.nomiss) 
stepAIC(oan.1200priming.2, direction = "both")$anova
##TargetFanIn

#semantics
oan.1200priming.3 = lm(NT.1200ms.RT.Priming ~ PCosine_Set + 
                         TCosine_Set + 
                         Proot_feat_set + 
                         Troot_feat_set +
                         jcn.y + root + 
                         affix + distance,
                       data = oan.nomiss)
stepAIC(oan.1200priming.3, direction = "both")$anova
##nothing

#thematics
oan.1200priming.4 = lm(NT.1200ms.RT.Priming ~ 
                         LSA, 
                       data = oan.nomiss)
stepAIC(oan.1200priming.4, direction = "both")$anova
##nothing

oan.1200priming.overall = lm(NT.1200ms.RT.Priming ~  
                               TLength + TOrthoN +
                               TargetFanIn,
                             data = oan.nomiss)
summary(oan.1200priming.overall, correlation = T)

#relative importance 
calc.relimp(oan.1200priming.overall, 
            type = c("lmg", "last", "first", "betasq"), 
            rela = TRUE)

boot.oan.1200prime = boot.relimp(oan.1200priming.overall, 
                                 b = 1000, 
                                 type = c("lmg", "last", "first", "betasq"),
                                 rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot.oan.1200prime) # print result
plot(booteval.relimp(boot.oan.1200prime,sort=TRUE))
