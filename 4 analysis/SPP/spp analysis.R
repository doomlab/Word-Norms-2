##setup
dat = read.csv("combined_spp_file.csv")

options(scipen = 999)

####models####
##t_ldt_RT
##first step (word info)
model1 = lm(t_ldt_RT ~ PLength + PSubFreq + POrthoN + TLength + TSubFreq + TOrthoN, data = dat) 
summary(model1, correlation = T)

##second step (association)
model2 = lm(t_ldt_RT ~ PLength + PSubFreq + POrthoN + TLength + TSubFreq + TOrthoN +
              FAS + BAS + PCueFanOut + TTargetFanIn, data = dat) 
summary(model2)
lm.beta(model2)

##third step (semantics)
model3 = lm(t_ldt_RT ~ PLength + PSubFreq + POrthoN + TLength + TSubFreq + TOrthoN +
              FAS + BAS + PCueFanOut + TTargetFanIn +
              jcn + root + raw + affix, data = dat) ##need to add in nelson stuff
summary(model3)

##fourth step (thematics)
model4 = lm(t_ldt_RT ~ PLength + PSubFreq + POrthoN + TLength + TSubFreq + TOrthoN +
              FAS + BAS + PCueFanOut + TTargetFanIn +
              jcn + root + raw + affix +
              LSA + BEAGLEcosine, data = dat) ##need to add in cosine/mandera stuff
summary(model4) 

##t_LDT_Z

##t_name_RT

##t_name_Z