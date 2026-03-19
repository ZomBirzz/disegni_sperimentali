#analisi della regressione

rm(list=ls(all=TRUE))
dataset <- read.table(file.choose(),header=T,sep=",",stringsAsFactors = T)

attach(dataset)
X=Concentrazione
Y=Fluorescenza
plot(X,Y)

linear.model <- lm(Y~X)
abline(linear.model, col="red") #formula generalizzata per fare la retta sul grafico

regression <- summary(linear.model) #ti da i test da fare sul modello lineare, in particolare fa il test per l'intercetta = 0 e per il coefficiente angolare = 0
#fa il t di Student per valutare la bontà dei parametri lineari, per campioni piccoli
#in un secondo momento fa l'f di Fisher che è un ANOVA, per molti punti

plot(linear.model) #volendo puoi plottare il modello lineare e avere altre informazioni su quello che vedi, ma non dovrebbe servire
#eventualmente, puoi fare dei test per calcolare il coefficiente lineare significativo rispetto ad un valore diverso da 0, utile per confrontare lavori diversi
#stesso concetto per l'intercetta

newX = X
predict(linear.model, data.frame(X=newX), level=0.95, interval="confidence")

par(mfrow=c(1,1)) #non lo so
newX = seq(from=min(X), to=max(X), by=((max(X)-min(X))/100)) #essenzialmente, ti permette di decidere l'intervallo in cui fare i puntini delle linee
bands=data.frame(predict(linear.model,data.frame(X=newX), level=0.95, interval="confidence")) #funzione per creare le linee di probabilità attorno alla retta
#da qui in giù mettiamo in un grafico quello calcolato nelle 3 righe prima
plot(X,Y)
abline(linear.model, col="red")
points(newX, bands$lwr, type="l", lty=2, col="blue")
points(newX, bands$upr, type="l", lty=2, col="blue")

linear.model <- lm(Y~0+X) #fai questa cosa per forzare la retta di regressione ad avere intercetta 0

detach(dataset)