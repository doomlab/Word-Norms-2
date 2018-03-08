##setup
dat = read.csv("OAN.csv")

options(scipen = 999)

####finding best columns to work with -- step 1 word info####
##tlength, plength, ppos, tpos, psub, tsub, portho, tortho

####finding best columns -- step 2 association####
##FAS, BAS

##qss
summary(dat[ , c(20, 66)]) ##these are identical, so i'll use cuefanout

##tss
summary(dat[, c( 46, 70)]) ##tss is a factor, and the numbers are way different
dat$TSS = as.numeric(as.character(dat$TSS)) 

summary(dat[, c( 46, 70)]) ##TSS also has more missing, so using targetfanin

####finding best columns -- step 3 semantics ####
##jcn
summary(dat[ , c(53, 83)])

##make jcn.x numeric
dat$jcn.x = as.numeric(as.character(dat$jcn.x))

summary(dat[ , c(53, 83)]) ##going to use jcn.y

##cos
##root, raw, affix, distance

####finding best columns -- step 4 thematics
##lsa
summary(dat[ , c(21, 54, 84)])
dat$lsa.x = as.numeric(as.character(dat$lsa.x))

summary(dat[ , c(21, 54, 84)]) ##using LSA column

##beagle?

##create new dataset of just columns being used
dat2 = dat[ , c(4:5, 8, 12, 18:35, 38, 42, 46, 49:51, 59, 83, 86:92)] ##getting just the columns for the analysis
nomiss = na.omit(dat2)

####analysis -- NT 200 rt####
##first step (word info)
model1 = lm(NT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
            data = nomiss) ##phonographic neighborhood?
summary(model1)

##second step (association)
model2 = lm(NT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              nomiss$BAS + nomiss$FAS + nomiss$CueFanOut + nomiss$TargetFanIn + nomiss$PCosine_Set + nomiss$TCosine_Set + nomiss$Praw_feat_set + nomiss$Traw_feat_set + nomiss$Proot_feat_set + nomiss$Troot_feat_set,
            data = nomiss) 
summary(model2)

##third step (semantics)
model3 = lm(NT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              nomiss$BAS + nomiss$FAS + nomiss$CueFanOut + nomiss$TargetFanIn + nomiss$PCosine_Set + nomiss$TCosine_Set + nomiss$Praw_feat_set + nomiss$Traw_feat_set + nomiss$Proot_feat_set + nomiss$Troot_feat_set +
              nomiss$jcn.y + nomiss$root + nomiss$raw + nomiss$affix + nomiss$distance,
            data = nomiss)
summary(model3)

##fourth step (thematics)
model4 = lm(NT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              nomiss$BAS + nomiss$FAS + nomiss$CueFanOut + nomiss$TargetFanIn + nomiss$PCosine_Set + nomiss$TCosine_Set + nomiss$Praw_feat_set + nomiss$Traw_feat_set + nomiss$Proot_feat_set + nomiss$Troot_feat_set +
              nomiss$jcn.y + nomiss$root + nomiss$raw + nomiss$affix + nomiss$distance +
              nomiss$LSA, ##beagle stuff?
            data = nomiss)
summary(model4)

####analysis NT 1200 rt####
##first step
model5 = lm(NT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
            data = nomiss) ##phonographic neighborhood?
summary(model5)

##second step (association)
model6 = lm(NT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              nomiss$BAS + nomiss$FAS + nomiss$CueFanOut + nomiss$TargetFanIn + nomiss$PCosine_Set + nomiss$TCosine_Set + nomiss$Praw_feat_set + nomiss$Traw_feat_set + nomiss$Proot_feat_set + nomiss$Troot_feat_set,
            data = nomiss) 
summary(model6)

##third step (semantics)
model7 = lm(NT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              nomiss$BAS + nomiss$FAS + nomiss$CueFanOut + nomiss$TargetFanIn + nomiss$PCosine_Set + nomiss$TCosine_Set + nomiss$Praw_feat_set + nomiss$Traw_feat_set + nomiss$Proot_feat_set + nomiss$Troot_feat_set +
              nomiss$jcn.y + nomiss$root + nomiss$raw + nomiss$affix + nomiss$distance,
            data = nomiss)
summary(model7)

##fourth step (thematics)
model8 = lm(NT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              nomiss$BAS + nomiss$FAS + nomiss$CueFanOut + nomiss$TargetFanIn + nomiss$PCosine_Set + nomiss$TCosine_Set + nomiss$Praw_feat_set + nomiss$Traw_feat_set + nomiss$Proot_feat_set + nomiss$Troot_feat_set +
              nomiss$jcn.y + nomiss$root + nomiss$raw + nomiss$affix + nomiss$distance +
              nomiss$LSA, ##beagle stuff?
            data = nomiss)
summary(model8)

####NT 200 rt priming####
##first step
model9 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
            data = nomiss) ##phonographic neighborhood?
summary(model9)

##second step (association)
model10 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
               nomiss$BAS + nomiss$FAS + nomiss$CueFanOut + nomiss$TargetFanIn + nomiss$PCosine_Set + nomiss$TCosine_Set + nomiss$Praw_feat_set + nomiss$Traw_feat_set + nomiss$Proot_feat_set + nomiss$Troot_feat_set,
             data = nomiss) 
summary(model10)

##third step (semantics)
model11 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
               nomiss$BAS + nomiss$FAS + nomiss$CueFanOut + nomiss$TargetFanIn + nomiss$PCosine_Set + nomiss$TCosine_Set + nomiss$Praw_feat_set + nomiss$Traw_feat_set + nomiss$Proot_feat_set + nomiss$Troot_feat_set +
               nomiss$jcn.y + nomiss$root + nomiss$raw + nomiss$affix + nomiss$distance,
             data = nomiss)
summary(model11)

##fourth step (thematics)
model12 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
               nomiss$BAS + nomiss$FAS + nomiss$CueFanOut + nomiss$TargetFanIn + nomiss$PCosine_Set + nomiss$TCosine_Set + nomiss$Praw_feat_set + nomiss$Traw_feat_set + nomiss$Proot_feat_set + nomiss$Troot_feat_set +
               nomiss$jcn.y + nomiss$root + nomiss$raw + nomiss$affix + nomiss$distance +
               nomiss$LSA, ##beagle stuff?
             data = nomiss)
summary(model12)

####NT 1200 rt priming####
##first step
model13 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
             data = nomiss) ##phonographic neighborhood?
summary(model13)

##second step (association)
model14 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
               nomiss$BAS + nomiss$FAS + nomiss$CueFanOut + nomiss$TargetFanIn + nomiss$PCosine_Set + nomiss$TCosine_Set + nomiss$Praw_feat_set + nomiss$Traw_feat_set + nomiss$Proot_feat_set + nomiss$Troot_feat_set,
             data = nomiss) 
summary(model14)

##third step (semantics)
model15 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
               nomiss$BAS + nomiss$FAS + nomiss$CueFanOut + nomiss$TargetFanIn + nomiss$PCosine_Set + nomiss$TCosine_Set + nomiss$Praw_feat_set + nomiss$Traw_feat_set + nomiss$Proot_feat_set + nomiss$Troot_feat_set +
               nomiss$jcn.y + nomiss$root + nomiss$raw + nomiss$affix + nomiss$distance,
             data = nomiss)
summary(model15)

##fourth step (thematics)
model16 = lm(NT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
               nomiss$BAS + nomiss$FAS + nomiss$CueFanOut + nomiss$TargetFanIn + nomiss$PCosine_Set + nomiss$TCosine_Set + nomiss$Praw_feat_set + nomiss$Traw_feat_set + nomiss$Proot_feat_set + nomiss$Troot_feat_set +
               nomiss$jcn.y + nomiss$root + nomiss$raw + nomiss$affix + nomiss$distance +
               nomiss$LSA, ##beagle stuff?
             data = nomiss)
summary(model16)

