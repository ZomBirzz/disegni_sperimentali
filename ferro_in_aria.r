rm(list=ls(all=TRUE))
dataset <- read.table(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
detach(dataset)
par(mai=c(2.5,1,.5,.5))

par(mai=c(2.5,1,.5,.5)) #genera più spazio per la scala del grafico 
boxplot(dataset[-1], las=2, ylab="Ricoprimento %", outline=FALSE) #[-1] serve a togliere la prima colonna, las=2 gira in verticale le scritte

par(mai=c(.8,.8,.2,.2))
boxplot(feltro.algale~SITO, xlab="Siti", ylab="Feltro algale (%)")
points(tapply(feltro.algale,SITO,mean),pch=16)

bartlett.test(feltro.algale~SITO) #visto che il p è troppo piccolo dobbiamo trasformare i dati

sqrt_data <- sqrt(feltro.algale)
bartlett.test(sqrt_data~SITO) #adesso il p è sopra 0.05

par(mfrow=c(1,2))
boxplot(feltro.algale~SITO); points(tapply(feltro.algale,SITO,mean),pch=16)
boxplot(sqrt_data~SITO); points(tapply(sqrt_data,SITO,mean),pch=16)

linear.model=lm(sqrt_data~SITO)
anova(linear.model)
