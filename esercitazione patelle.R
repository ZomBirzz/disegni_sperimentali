rm(list=ls(all=TRUE)) # cancellazione memoria
dataset <- read.csv(file.choose(),header=T,sep=",") # importazione dataset
attach(dataset)
detach(dataset)

summary(dataset)
ricci <- factor(ricci)

library(GAD)
ricci <- as.fixed(ricci)
patch <- as.random(patch)

linear.model = lm(alghe~ricci+patch%in%ricci)

C.test(linear.model)

boxplot(alghe~ricci+patch)

gad(linear.model)

