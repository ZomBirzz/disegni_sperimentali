rm(list=ls(all=TRUE)) # cancellazione memoria
dataset <- read.csv(file.choose(),header=T,sep=",") # importazione dataset
attach(dataset)
detach(dataset)

dataset
#fattori fissi incrociati

library(GAD)
mangime <- as.fixed(mangime)
sesso <- as.fixed(sesso)

linear.model = lm(accrescimento~mangime+sesso+mangime*sesso)

boxplot(accrescimento~mangime+sesso, ylab="Sesso x Mangime, accrescimento (g)")
points(tapply(accrescimento,paste(mangime,sesso),mean),pch=16)

C.test(linear.model)
#accrescimento = sqrt(accrescimento)

gad(linear.model)
snk.test(linear.model,term='mangime:sesso',among='mangime',within='sesso',anova.tab=gad(linear.model))
snk.test(linear.model,term='mangime:sesso',among='sesso',within='mangime',anova.tab=gad(linear.model))
