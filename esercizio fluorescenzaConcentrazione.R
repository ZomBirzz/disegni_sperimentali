rm(list=ls(all=TRUE)) # cancellazione memoria
dataset <- read.csv(file.choose(),header=T,sep=",") # importazione dataset 011
attach(dataset)

summary(dataset)
X = Concentrazione
Y = Fluorescenza

linear.model <- lm(Y~X)

regression = summary(linear.model)
regression

confint(linear.model, level=0.95)

newX = seq(from=min(X), to=max(X), by=((max(X)-min(X))/100))
bands=data.frame(predict(linear.model,data.frame(X=newX), level=0.95, interval="confidence"))

plot(X,Y, xlab="Concentrazione pg/ml", ylab = "Fluorescenza")
abline(linear.model, col="red")
points(newX, bands$lwr, type="l", lty=2, col="blue")
points(newX, bands$upr, type="l", lty=2, col="blue")

formula <- paste("y = ", formatC(linear.model[["coefficients"]][["X"]], digits = 4), " x + ", formatC(linear.model[["coefficients"]][["(Intercept)"]], digits = 4))
text(locator(1), formula)


detach(dataset)