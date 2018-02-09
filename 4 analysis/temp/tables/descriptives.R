setwd("~/OneDrive/2 projects/Word Norms 2/4 analysis/")
des <- read.csv("descriptives.csv")
with(des, tapply(average2, list(word.type, school.code), mean))
with(des, tapply(average2, list(word.type), mean))
with(des, tapply(average2, list(school.code), mean))

with(des, tapply(average2, list(word.type, school.code), sd))
with(des, tapply(average2, list(word.type), sd))
with(des, tapply(average2, list(school.code), sd))

percent = with(des, tapply(average2, list(word.type), length))
percent / 3722 * 100
