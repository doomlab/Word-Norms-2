master = read.csv("updated 2017 words.csv")
View(master)
library(memisc)

##total percents
p0a = percent(master$pos_feature)
p0a

p0b = percent(master$pos_translated)
p0b

m0a = tapply(master$normalized_feature,
             master$pos_feature, mean)
m0a

sd0a = tapply(master$normalized_feature,
              master$pos_feature, sd)
sd0a

m0b = tapply(master$normalized_translated,
             master$pos_translated, mean)
m0b

sd0b = tapply(master$normalized_translated,
              master$pos_translated, sd)
sd0b

##subsetting by cue type
adj = subset(master,
             master$pos_cue == "adjective")

noun = subset(master,
              master$pos_cue == "noun")

verb = subset(master,
              master$pos_cue == "verb")

other = subset(master,
               master$pos_cue == "other")              

##adjectives
p1a = percent(adj$pos_feature)
p1a

p1b = percent(adj$pos_translated)
p1b

m1a = tapply(adj$normalized_feature,
             adj$pos_feature, mean)
m1a

sd1a = tapply(adj$normalized_feature,
              adj$pos_feature, sd)
sd1a

m1b = tapply(adj$normalized_translated,
             adj$pos_translated, mean)
m1b

sd1b = tapply(adj$normalized_translated,
              adj$pos_translated, sd)
sd1b

##nouns
p2a = percent(noun$pos_feature)
p2a

p2b = percent(noun$pos_translated)
p2b

m2a = tapply(noun$normalized_feature,
            noun$pos_feature, mean)
m2a

sd2a = tapply(noun$normalized_feature,
              noun$pos_feature, sd)
sd2a

m2b = tapply(noun$normalized_translated,
             noun$pos_translated, mean)
m2b

sd2b = tapply(noun$normalized_translated,
              noun$pos_translated, sd)
sd2b

##verbs
p3a = percent(verb$pos_feature)
p3a

p3b = percent(verb$pos_translated)
p3b

m3a = tapply(verb$normalized_feature,
             verb$pos_feature, mean)
m3a

sd3a = tapply(verb$normalized_feature,
              verb$pos_feature, sd)
sd3a

m3b = tapply(verb$normalized_translated,
             verb$pos_translated, mean)
m3b

sd3b = tapply(verb$normalized_translated,
              verb$pos_feature, sd)
sd3b

##other
p4a = percent(other$pos_feature)
p4a

p4b = percent(other$pos_translated)
p4b

m4a = tapply(other$normalized_feature,
             other$pos_feature, mean)
m4a

sd4a = tapply(other$normalized_feature,
              other$pos_feature, sd)
sd4a

m4b = tapply(other$normalized_translated,
             other$pos_translated, mean)
m4b

sd4b = tapply(other$normalized_translated,
              other$pos_translated, sd)
sd4b
