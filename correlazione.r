rm(list=ls(all=TRUE))
dataset <- read.table(file.choose(),header=T,sep=",",stringsAsFactors = T) #dataset 13
attach(dataset)
detach(dataset) #RICORDATI DI PULIRE

X1=Conducibilita
X2=Ioni
plot(X1,X2)

cor(X1,X2) #funzione per calcolare il coefficiente di correlazione, anche per correlazioni non monotoniche
cor(X1,X2,method="kendall") #correlazione di kendall, non fatto
cor(X1,X2,method="spearman") #correlazione di spearman, solo per correlazioni monotoniche

cor.test(X1,X2) #test bidirezionale, da specificare il method
#puoi fare un test unidirezionale con alternative = "less" o alternative = "greater"

#dataset 14
boxplot(dataset)
plot(Cu,Zn)

cor(data.frame(Cr,Cu,Ni,Zn,Pb,Hg)) #in questo modo fai la correlazione tra tutte le variabili che vuoi
pairs(~Cr+Cu+Ni+Zn+Pb+Hg) #per fare il grafico

install.packages("Hmisc") #libreria con cose carine come creare delle tabelle già pronte

Results = rcorr(as.matrix(dataset),type="pearson")
write.table(Results$r, "Pearson.r.csv", sep=",")
write.table(Results$n, "Pearson.n.csv", sep=",")
write.table(Results$P, "Pearson.P.csv", sep=",")