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


##faldt 1200RT priming
faldt.1200priming.1 = lm(LDT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                        data = faldt.nomiss) 
summary(faldt.1200priming.1, correlation = T)

##add associations
faldt.1200priming.2 = lm(LDT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                          PSubFreq + PPOS + TPOS + TSubFreq +
                          BAS + FAS + CueFanOut + 
                          TargetFanIn,
                        data = faldt.nomiss) 
summary(faldt.1200priming.2, correlation = T)

##add semantics
faldt.1200priming.3 = lm(LDT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                          PSubFreq + PPOS + TPOS + TSubFreq +
                          BAS + FAS + CueFanOut + 
                          TargetFanIn + PCosine_Set + 
                          TCosine_Set + 
                          Proot_feat_set + 
                          Troot_feat_set +
                          jcn.y + root + 
                          affix + distance,
                        data = faldt.nomiss)
summary(faldt.1200priming.3, correlation = T)

##add thematics
faldt.1200priming.4 = lm(LDT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                          PSubFreq + PPOS + TPOS + TSubFreq +
                          BAS + FAS + CueFanOut + 
                          TargetFanIn + PCosine_Set + 
                          TCosine_Set + 
                          Proot_feat_set + 
                          Troot_feat_set +
                          jcn.y + root + 
                          affix + distance +
                          LSA, 
                        data = faldt.nomiss)
summary(faldt.1200priming.4, correlation = T)

####Fan####
##fan 200RT
##lexical measures
fan.200RT.1 = lm(NT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                   data = fan.nomiss) 
summary(fan.200RT.1, correlation = T)

##add associations
fan.200RT.2 = lm(NT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                     PSubFreq + PPOS + TPOS + TSubFreq +
                     BAS + FAS + CueFanOut + 
                     TargetFanIn,
                   data = fan.nomiss) 
summary(fan.200RT.2, correlation = T)

##add semantics
fan.200RT.3 = lm(NT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                     PSubFreq + PPOS + TPOS + TSubFreq +
                     BAS + FAS + CueFanOut + 
                     TargetFanIn + PCosine_Set + 
                     TCosine_Set + 
                     Proot_feat_set + 
                     Troot_feat_set +
                     jcn.y + root + 
                     affix + distance,
                   data = fan.nomiss)
summary(fan.200RT.3, correlation = T)

##add thematics
fan.200RT.4 = lm(NT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                     PSubFreq + PPOS + TPOS + TSubFreq +
                     BAS + FAS + CueFanOut + 
                     TargetFanIn + PCosine_Set + 
                     TCosine_Set + 
                     Proot_feat_set + 
                     Troot_feat_set +
                     jcn.y + root + 
                     affix + distance +
                     LSA, 
                   data = fan.nomiss)
summary(fan.200RT.4, correlation = T)

##fan 1200RT
fan.1200RT.1 = lm(NT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                    data = fan.nomiss) 
summary(fan.200RT.1, correlation = T)

##add associations
fan.1200RT.2 = lm(NT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                      PSubFreq + PPOS + TPOS + TSubFreq +
                      BAS + FAS + CueFanOut + 
                      TargetFanIn,
                    data = fan.nomiss) 
summary(fan.1200RT.2, correlation = T)

##add semantics
fan.1200RT.3 = lm(NT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                      PSubFreq + PPOS + TPOS + TSubFreq +
                      BAS + FAS + CueFanOut + 
                      TargetFanIn + PCosine_Set + 
                      TCosine_Set + 
                      Proot_feat_set + 
                      Troot_feat_set +
                      jcn.y + root + 
                      affix + distance,
                    data = fan.nomiss)
summary(fan.1200RT.3, correlation = T)

##add thematics
fan.1200RT.4 = lm(NT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                      PSubFreq + PPOS + TPOS + TSubFreq +
                      BAS + FAS + CueFanOut + 
                      TargetFanIn + PCosine_Set + 
                      TCosine_Set + 
                      Proot_feat_set + 
                      Troot_feat_set +
                      jcn.y + root + 
                      affix + distance +
                      LSA, 
                    data = fan.nomiss)
summary(fan.1200RT.4, correlation = T)

##fan 200RT priming
fan.200priming.1 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                        data = fan.nomiss) 
summary(fan.200priming.1, correlation = T)

##add associations
fan.200priming.2 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                          PSubFreq + PPOS + TPOS + TSubFreq +
                          BAS + FAS + CueFanOut + 
                          TargetFanIn,
                        data = fan.nomiss) 
summary(fan.200priming.2, correlation = T)

##add semantics
fan.200priming.3 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                          PSubFreq + PPOS + TPOS + TSubFreq +
                          BAS + FAS + CueFanOut + 
                          TargetFanIn + PCosine_Set + 
                          TCosine_Set + 
                          Proot_feat_set + 
                          Troot_feat_set +
                          jcn.y + root + 
                          affix + distance,
                        data = fan.nomiss)
summary(fan.200priming.3, correlation = T)

##add thematics
fan.200priming.4 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                          PSubFreq + PPOS + TPOS + TSubFreq +
                          BAS + FAS + CueFanOut + 
                          TargetFanIn + PCosine_Set + 
                          TCosine_Set + 
                          Proot_feat_set + 
                          Troot_feat_set +
                          jcn.y + root + 
                          affix + distance +
                          LSA, 
                        data = fan.nomiss)
summary(fan.200priming.4, correlation = T)

##fan 1200RT priming
fan.1200priming.1 = lm(NT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                         data = fan.nomiss) 
summary(fan.1200priming.1, correlation = T)

##add associations
fan.1200priming.2 = lm(NT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                           PSubFreq + PPOS + TPOS + TSubFreq +
                           BAS + FAS + CueFanOut + 
                           TargetFanIn,
                         data = fan.nomiss) 
summary(fan.1200priming.2, correlation = T)

##add semantics
fan.1200priming.3 = lm(NT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                           PSubFreq + PPOS + TPOS + TSubFreq +
                           BAS + FAS + CueFanOut + 
                           TargetFanIn + PCosine_Set + 
                           TCosine_Set + 
                           Proot_feat_set + 
                           Troot_feat_set +
                           jcn.y + root + 
                           affix + distance,
                         data = fan.nomiss)
summary(fan.1200priming.3, correlation = T)

##add thematics
fan.1200priming.4 = lm(NT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                           PSubFreq + PPOS + TPOS + TSubFreq +
                           BAS + FAS + CueFanOut + 
                           TargetFanIn + PCosine_Set + 
                           TCosine_Set + 
                           Proot_feat_set + 
                           Troot_feat_set +
                           jcn.y + root + 
                           affix + distance +
                           LSA, 
                         data = fan.nomiss)
summary(fan.1200priming.4, correlation = T)

####oaldt#####
##oaldt 200RT
##lexical measures
oaldt.200RT.1 = lm(LDT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                   data = oaldt.nomiss) 
summary(oaldt.200RT.1, correlation = T)

##add associations
oaldt.200RT.2 = lm(LDT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                     PSubFreq + PPOS + TPOS + TSubFreq +
                     BAS + FAS + CueFanOut + 
                     TargetFanIn,
                   data = oaldt.nomiss) 
summary(oaldt.200RT.2, correlation = T)

##add semantics
oaldt.200RT.3 = lm(LDT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                     PSubFreq + PPOS + TPOS + TSubFreq +
                     BAS + FAS + CueFanOut + 
                     TargetFanIn + PCosine_Set + 
                     TCosine_Set + 
                     Proot_feat_set + 
                     Troot_feat_set +
                     jcn.y + root + 
                     affix + distance,
                   data = oaldt.nomiss)
summary(oaldt.200RT.3, correlation = T)

##add thematics
oaldt.200RT.4 = lm(LDT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                     PSubFreq + PPOS + TPOS + TSubFreq +
                     BAS + FAS + CueFanOut + 
                     TargetFanIn + PCosine_Set + 
                     TCosine_Set + 
                     Proot_feat_set + 
                     Troot_feat_set +
                     jcn.y + root + 
                     affix + distance +
                     LSA, 
                   data = oaldt.nomiss)
summary(oaldt.200RT.4, correlation = T)

##oaldt 1200RT
oaldt.1200RT.1 = lm(LDT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                    data = oaldt.nomiss) 
summary(oaldt.1200RT.1, correlation = T)

##add associations
oaldt.1200RT.2 = lm(LDT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                      PSubFreq + PPOS + TPOS + TSubFreq +
                      BAS + FAS + CueFanOut + 
                      TargetFanIn,
                    data = oaldt.nomiss) 
summary(oaldt.1200RT.2, correlation = T)

##add semantics
oaldt.1200RT.3 = lm(LDT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                      PSubFreq + PPOS + TPOS + TSubFreq +
                      BAS + FAS + CueFanOut + 
                      TargetFanIn + PCosine_Set + 
                      TCosine_Set + 
                      Proot_feat_set + 
                      Troot_feat_set +
                      jcn.y + root + 
                      affix + distance,
                    data = oaldt.nomiss)
summary(oaldt.1200RT.3, correlation = T)

##add thematics
oaldt.1200RT.4 = lm(LDT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                      PSubFreq + PPOS + TPOS + TSubFreq +
                      BAS + FAS + CueFanOut + 
                      TargetFanIn + PCosine_Set + 
                      TCosine_Set + 
                      Proot_feat_set + 
                      Troot_feat_set +
                      jcn.y + root + 
                      affix + distance +
                      LSA, 
                    data = oaldt.nomiss)
summary(oaldt.1200RT.4, correlation = T)

##oaldt 200RT priming
oaldt.200priming.1 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                        data = oaldt.nomiss) 
summary(oaldt.200priming.1, correlation = T)

##add associations
oaldt.200priming.2 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                          PSubFreq + PPOS + TPOS + TSubFreq +
                          BAS + FAS + CueFanOut + 
                          TargetFanIn,
                        data = oaldt.nomiss) 
summary(oaldt.200priming.2, correlation = T)

##add semantics
oaldt.200priming.3 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                          PSubFreq + PPOS + TPOS + TSubFreq +
                          BAS + FAS + CueFanOut + 
                          TargetFanIn + PCosine_Set + 
                          TCosine_Set + 
                          Proot_feat_set + 
                          Troot_feat_set +
                          jcn.y + root + 
                          affix + distance,
                        data = oaldt.nomiss)
summary(oaldt.200priming.3, correlation = T)

##add thematics
oaldt.200priming.4 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                          PSubFreq + PPOS + TPOS + TSubFreq +
                          BAS + FAS + CueFanOut + 
                          TargetFanIn + PCosine_Set + 
                          TCosine_Set + 
                          Proot_feat_set + 
                          Troot_feat_set +
                          jcn.y + root + 
                          affix + distance +
                          LSA, 
                        data = oaldt.nomiss)
summary(oaldt.200priming.4, correlation = T)

##oaldt 1200RT priming
oaldt.1200priming.1 = lm(LDT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                         data = oaldt.nomiss) 
summary(oaldt.1200priming.1, correlation = T)

##add associations
oaldt.1200priming.2 = lm(LDT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                           PSubFreq + PPOS + TPOS + TSubFreq +
                           BAS + FAS + CueFanOut + 
                           TargetFanIn,
                         data = oaldt.nomiss) 
summary(oaldt.1200priming.2, correlation = T)

##add semantics
oaldt.1200priming.3 = lm(LDT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                           PSubFreq + PPOS + TPOS + TSubFreq +
                           BAS + FAS + CueFanOut + 
                           TargetFanIn + PCosine_Set + 
                           TCosine_Set + 
                           Proot_feat_set + 
                           Troot_feat_set +
                           jcn.y + root + 
                           affix + distance,
                         data = oaldt.nomiss)
summary(oaldt.1200priming.3, correlation = T)

##add thematics
oaldt.1200priming.4 = lm(LDT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                           PSubFreq + PPOS + TPOS + TSubFreq +
                           BAS + FAS + CueFanOut + 
                           TargetFanIn + PCosine_Set + 
                           TCosine_Set + 
                           Proot_feat_set + 
                           Troot_feat_set +
                           jcn.y + root + 
                           affix + distance +
                           LSA, 
                         data = oaldt.nomiss)
summary(oaldt.1200priming.4, correlation = T)

####oan####
##oan 200RT
##lexical measures
oan.200RT.1 = lm(NT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                 data = oan.nomiss) 
summary(oan.200RT.1, correlation = T)

##add associations
oan.200RT.2 = lm(NT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                   PSubFreq + PPOS + TPOS + TSubFreq +
                   BAS + FAS + CueFanOut + 
                   TargetFanIn,
                 data = oan.nomiss) 
summary(oan.200RT.2, correlation = T)

##add semantics
oan.200RT.3 = lm(NT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                   PSubFreq + PPOS + TPOS + TSubFreq +
                   BAS + FAS + CueFanOut + 
                   TargetFanIn + PCosine_Set + 
                   TCosine_Set + 
                   Proot_feat_set + 
                   Troot_feat_set +
                   jcn.y + root + 
                   affix + distance,
                 data = oan.nomiss)
summary(oan.200RT.3, correlation = T)

##add thematics
oan.200RT.4 = lm(NT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                   PSubFreq + PPOS + TPOS + TSubFreq +
                   BAS + FAS + CueFanOut + 
                   TargetFanIn + PCosine_Set + 
                   TCosine_Set + 
                   Proot_feat_set + 
                   Troot_feat_set +
                   jcn.y + root + 
                   affix + distance +
                   LSA, 
                 data = oan.nomiss)
summary(oan.200RT.4, correlation = T)

##oan 1200RT
oan.1200RT.1 = lm(NT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                  data = oan.nomiss) 
summary(oan.200RT.1, correlation = T)

##add associations
oan.1200RT.2 = lm(NT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                    PSubFreq + PPOS + TPOS + TSubFreq +
                    BAS + FAS + CueFanOut + 
                    TargetFanIn,
                  data = oan.nomiss) 
summary(oan.1200RT.2, correlation = T)

##add semantics
oan.1200RT.3 = lm(NT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                    PSubFreq + PPOS + TPOS + TSubFreq +
                    BAS + FAS + CueFanOut + 
                    TargetFanIn + PCosine_Set + 
                    TCosine_Set + 
                    Proot_feat_set + 
                    Troot_feat_set +
                    jcn.y + root + 
                    affix + distance,
                  data = oan.nomiss)
summary(oan.1200RT.3, correlation = T)

##add thematics
oan.1200RT.4 = lm(NT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + 
                    PSubFreq + PPOS + TPOS + TSubFreq +
                    BAS + FAS + CueFanOut + 
                    TargetFanIn + PCosine_Set + 
                    TCosine_Set + 
                    Proot_feat_set + 
                    Troot_feat_set +
                    jcn.y + root + 
                    affix + distance +
                    LSA, 
                  data = oan.nomiss)
summary(oan.1200RT.4, correlation = T)

##oan 200RT priming
oan.200priming.1 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                      data = oan.nomiss) 
summary(oan.200priming.1, correlation = T)

##add associations
oan.200priming.2 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                        PSubFreq + PPOS + TPOS + TSubFreq +
                        BAS + FAS + CueFanOut + 
                        TargetFanIn,
                      data = oan.nomiss) 
summary(oan.200priming.2, correlation = T)

##add semantics
oan.200priming.3 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                        PSubFreq + PPOS + TPOS + TSubFreq +
                        BAS + FAS + CueFanOut + 
                        TargetFanIn + PCosine_Set + 
                        TCosine_Set + 
                        Proot_feat_set + 
                        Troot_feat_set +
                        jcn.y + root + 
                        affix + distance,
                      data = oan.nomiss)
summary(oan.200priming.3, correlation = T)

##add thematics
oan.200priming.4 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                        PSubFreq + PPOS + TPOS + TSubFreq +
                        BAS + FAS + CueFanOut + 
                        TargetFanIn + PCosine_Set + 
                        TCosine_Set + 
                        Proot_feat_set + 
                        Troot_feat_set +
                        jcn.y + root + 
                        affix + distance +
                        LSA, 
                      data = oan.nomiss)
summary(oan.200priming.4, correlation = T)

##oan 1200RT priming
oan.1200priming.1 = lm(NT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
                       data = oan.nomiss) 
summary(oan.1200priming.1, correlation = T)

##add associations
oan.1200priming.2 = lm(NT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                         PSubFreq + PPOS + TPOS + TSubFreq +
                         BAS + FAS + CueFanOut + 
                         TargetFanIn,
                       data = oan.nomiss) 
summary(oan.1200priming.2, correlation = T)

##add semantics
oan.1200priming.3 = lm(NT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                         PSubFreq + PPOS + TPOS + TSubFreq +
                         BAS + FAS + CueFanOut + 
                         TargetFanIn + PCosine_Set + 
                         TCosine_Set + 
                         Proot_feat_set + 
                         Troot_feat_set +
                         jcn.y + root + 
                         affix + distance,
                       data = oan.nomiss)
summary(oan.1200priming.3, correlation = T)

##add thematics
oan.1200priming.4 = lm(NT.1200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + 
                         PSubFreq + PPOS + TPOS + TSubFreq +
                         BAS + FAS + CueFanOut + 
                         TargetFanIn + PCosine_Set + 
                         TCosine_Set + 
                         Proot_feat_set + 
                         Troot_feat_set +
                         jcn.y + root + 
                         affix + distance +
                         LSA, 
                       data = oan.nomiss)
summary(oan.1200priming.4, correlation = T)

