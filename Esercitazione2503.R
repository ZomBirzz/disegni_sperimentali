rm(list=ls(all=TRUE)) # cancellazione memoria
dataset <- read.csv(file.choose(),header=T,sep=",") # importazione dataset

attach(dataset)
detach(dataset)

X1=Temperatura
X2=S
plot(X1,X2, xlab="Temperatura °C", ylab="Salinità PSU", main="Correlazione Temperatura-Salinità")

cor.test(X1,X2)