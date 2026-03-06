rm(list=ls(all=TRUE))
dataset <- read.table(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
head(dataset)
summary(dataset)
boxplot(crescita~luce, xlab="Tipo di luce",ylab="Crescita %"); points(tapply(crescita,luce,mean),pch=16)

bartlett.test(crescita~luce)

linear_model <- lm(crescita~luce)
anova(linear_model)

TukeyHSD(aov(crescita~luce)) # aov scompone l'analisi
detach(dataset)
