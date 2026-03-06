rm(list=ls(all=TRUE))
dataset <- read.table(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
dataset$S

se <- function(x) sd(x)/sqrt(length(x)) 
values <- tapply(S, Esposition, mean)
errors <- tapply(S, Esposition, se)
x <- barplot(values, ylim=c(0, max(S) + max(errors)*1.1),ylab=expression("Richness"), xlab = "Exposition")
arrows(x, values, x, values+errors, code=3, angle=90, length=.1) # crea le barre di errore
box()

par(mai=c(1,1,1,1))
boxplot(S~Esposition, varwidth=T, ylab=expression("Exposition"))
points(tapply(S,Esposition,mean),pch=16)

bartlett.test(S~Esposition)

linear.model=lm(S~Esposition)
anova(linear.model)

