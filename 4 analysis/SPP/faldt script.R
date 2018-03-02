##setup
dat = read.csv("FALDT.csv")

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

####analysis -- ldt 200 rt####
##first step (word info)
model1 = lm(LDT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
            data = dat) ##phonographic neighborhood?
summary(model1)

##second step (association)
model2 = lm(LDT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              dat$BAS + dat$FAS + dat$CueFanOut + dat$TargetFanIn,
            data = dat) 
summary(model2)

##third step (semantics)
model3 = lm(LDT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              dat$BAS + dat$FAS + dat$CueFanOut + dat$TargetFanIn +
              dat$jcn.y + dat$root + dat$raw + dat$affix + dat$distance,
            data = dat)
summary(model3)

##fourth step (thematics)
model4 = lm(LDT.200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              dat$BAS + dat$FAS + dat$CueFanOut + dat$TargetFanIn +
              dat$jcn.y + dat$root + dat$raw + dat$affix + dat$distance +
              dat$LSA, ##beagle stuff?
            data = dat)
summary(model4)

####analysis ldt 1200 rt####
##first step
model5 = lm(LDT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
            data = dat) ##phonographic neighborhood?
summary(model5)

##second step (association)
model6 = lm(LDT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              dat$BAS + dat$FAS + dat$CueFanOut + dat$TargetFanIn,
            data = dat) 
summary(model6)

##third step (semantics)
model7 = lm(LDT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              dat$BAS + dat$FAS + dat$CueFanOut + dat$TargetFanIn +
              dat$jcn.y + dat$root + dat$raw + dat$affix + dat$distance,
            data = dat)
summary(model7)

##fourth step (thematics)
model8 = lm(LDT.1200ms.RT ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              dat$BAS + dat$FAS + dat$CueFanOut + dat$TargetFanIn +
              dat$jcn.y + dat$root + dat$raw + dat$affix + dat$distance +
              dat$LSA, ##beagle stuff?
            data = dat)
summary(model8)

####ldt 200 rt priming####
##first step
model9 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
            data = dat) ##phonographic neighborhood?
summary(model9)

##second step (association)
model10 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              dat$BAS + dat$FAS + dat$CueFanOut + dat$TargetFanIn,
            data = dat) 
summary(model10)

##third step (semantics)
model11 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              dat$BAS + dat$FAS + dat$CueFanOut + dat$TargetFanIn +
              dat$jcn.y + dat$root + dat$raw + dat$affix + dat$distance,
            data = dat)
summary(model11)

##fourth step (thematics)
model12 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
              dat$BAS + dat$FAS + dat$CueFanOut + dat$TargetFanIn +
              dat$jcn.y + dat$root + dat$raw + dat$affix + dat$distance +
              dat$LSA, ##beagle stuff?
            data = dat)
summary(model12)

####ldt 1200 rt priming####
##first step
model13 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq,
            data = dat) ##phonographic neighborhood?
summary(model13)

##second step (association)
model14 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
               dat$BAS + dat$FAS + dat$CueFanOut + dat$TargetFanIn,
             data = dat) 
summary(model14)

##third step (semantics)
model15 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
               dat$BAS + dat$FAS + dat$CueFanOut + dat$TargetFanIn +
               dat$jcn.y + dat$root + dat$raw + dat$affix + dat$distance,
             data = dat)
summary(model15)

##fourth step (thematics)
model16 = lm(LDT.200ms.RT.Priming ~ PLength + TLength + POrthoN + TOrthoN + PSubFreq + PPOS + TPOS + TSubFreq +
               dat$BAS + dat$FAS + dat$CueFanOut + dat$TargetFanIn +
               dat$jcn.y + dat$root + dat$raw + dat$affix + dat$distance +
               dat$LSA, ##beagle stuff?
             data = dat)
summary(model16)
