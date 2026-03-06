rm(list=ls(all=TRUE))
install.packages("GAD")

library(GAD)

dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)

color<-c("yellow","red")
boxplot(EGGS~SEASON+DENSITY, ylab=expression(n~masses~ind^-1),col=color) 
points(tapply(EGGS,paste(DENSITY,SEASON),mean),pch=16) #sono invertiti per un qualche motivo ma non capisco, è corretto
points(tapply(EGGS,paste(SEASON,DENSITY),mean),pch=16)


SEASON <- as.fixed(SEASON) #as.fixed setta il fattore come fisso, esiste anche as.random per segnare che è random
DENSITY <- as.fixed(DENSITY)
linear.model=lm(EGGS~SEASON+DENSITY+SEASON*DENSITY) #dato che teniamo conto di 2 fattori, season e density, letteralmente il modello lineare con l'interazione
estimates(linear.model) #ti dice il denominatore per il test F corretto da utilizzare

C.test(linear.model) #cockran my beloved

gad(linear.model) #ANOVA two way già impostato!
snk.test(linear.model,term='SEASON',anova.tab=gad(linear.model)) #test post doc, ci piace. NB avendo season 2 livelli il post doc non serviva
snk.test(linear.model,term='DENSITY',anova.tab=gad(linear.model)) #snk.test procede coi ranghi, quindi le righe dell'output di mostrano sempre meno interazioni, il risultato è quello tipico dell'effetto di un gradiente (non si esplicita tra campioni adiacenti)


detach(dataset)