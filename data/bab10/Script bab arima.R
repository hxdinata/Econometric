simarima1 <-  arima.sim(n= 100, list(order=c(2,1,1), ar=c(0.5,-0.5),ma=-0.3)) 
ts.plot(simarima1,col="blue",main="Time Series Plot")
plot(ARMAacf(ar=c(0.9,-0.5),ma=c(0.4), lag.max = 20),type="h")
abline(h=0)
%===
setwd("c:\\data\\bab10")
latihan3=read.table("data.txt",header=TRUE, sep="\t", 
                na.strings="NA", dec=".", strip.white=TRUE)
#plot time series
latihan3$World_Oil_Prices <- ts(latihan3$World_Oil_Prices,start=c(1996,1),
  freq=12) # data diubah menjadi bertipe time series
ts.plot(latihan3$World_Oil_Prices,col="blue",main="Time Series Plot") 

# Uji stasioneritas
 adf.test(latihan3$World_Oil_Prices)
 win.graph()
 par(mfrow=c(2,1))
 acf(latihan3$World_Oil_Prices,na.action=na.pass)
 pacf(latihan3$World_Oil_Prices,na.action=na.pass)

#Transformasi data
latihan3$World_Oil_Prices.Diff1 <- diff(latihan3$World_Oil_Prices, diff=1)
ts.plot(latihan3$World_Oil_Prices.Diff1,col="blue",main="Time Series Plot")
latihan3$World_Oil_Prices.Difflog1 <- diff(log(latihan3$World_Oil_Prices), differences=1)
ts.plot(latihan3$World_Oil_Prices.Difflog1,col="blue",main="Time Series Plot")

#Identifikasi order
par(mfrow=c(1,2))
acf(latihan3$World_Oil_Prices.Log.Diff1,lag.max=36,na.action=na.pass)
pacf(latihan3$World_Oil_Prices.Log.Diff1,lag.max=36,na.action=na.pass)

#Estimasi model

library(forecast)
World_Oil_Prices.Log <- log(latihan3$World_Oil_Prices)
World_Oil_Prices.Log <- ts(World_Oil_Prices.Log,start=c(1996,1),freq=12) 
#model 1
ArimaModel.1 <- Arima(World_Oil_Prices.Log,order=c(1,1,0), seasonal=list(order=c(0,0,0),period=NA),include.mean=FALSE)
summary(ArimaModel.1)
qt(c(0.025), df=155, lower.tail=F)
printstatarima(ArimaModel.1)
tsdiag(ArimaModel.1)
acfStat(ArimaModel.1$residual)


#Model 2
ArimaModel.2 <- Arima(World_Oil_Prices.Log,order=c(0,1,1),
               seasonal=list(order=c(0,0,0),period=NA),include.mean=FALSE)
summary(ArimaModel.2)
printstatarima(ArimaModel.2)
tsdiag(ArimaModel.2)
acfStat(ArimaModel.2$residual)

#Model 3: 
ArimaModel.3 <- Arima(World_Oil_Prices.Log,order=c(1,1,1),
               seasonal=list(order=c(0,0,0),period=NA),include.mean=FALSE)
summary(ArimaModel.3)
printstatarima(ArimaModel.3)
tsdiag(ArimaModel.3)
acfStat(ArimaModel.3$residual)

#Model 4:
ArimaModel.4 <- Arima(World_Oil_Prices.Log,order=c(2,1,0),
               seasonal=list(order=c(0,0,0),period=NA),include.mean=FALSE)
summary(ArimaModel.4)
printstatarima(ArimaModel.4)
tsdiag(ArimaModel.4)
acfStat(ArimaModel.4$residual)

#Model 5: 
ArimaModel.5 <- Arima(World_Oil_Prices.Log,order=c(0,1,2),
               seasonal=list(order=c(0,0,0),period=NA),include.mean=FALSE)
summary(ArimaModel.5)
printstatarima(ArimaModel.5)
tsdiag(ArimaModel.5)
acfStat(ArimaModel.5$residual)

#prediksi dengan model terbaik
library(forecast)
World_Oil_Prices.Log <- log(latihan3$World_Oil_Prices)
World_Oil_Prices.Log <- ts(World_Oil_Prices.Log,start=c(1996,1),freq=12) 
#model 1
ArimaModel.1 <- Arima(World_Oil_Prices.Log,order=c(1,1,0), seasonal=list(order=c(0,0,0),period=NA),include.mean=FALSE)
summary(ArimaModel.1)
n.ahead.predict=6
pred.data = predict(ArimaModel.1, n.ahead = n.ahead.predict) #prediksi 6 langkah kedepan untuk log data
pred.data.low = pred.data$pred - 1.96 * pred.data$se
pred.data.up = pred.data$pred + 1.96 * pred.data$se

#transformasi antilog
pred.data=exp(pred.data$pred)
pred.data.low=exp(pred.data.low)
pred.data.up=exp(pred.data.up)

#menghitung fitted data dari model terbaik
fit.data = fitted(ArimaModel.1) #menghitung nilai fitting untuk log(World_Oil_Prices)
fit.data=exp(fit.data) #menghitung nilai fitting untuk World_Oil_Prices

#plot data
World_Oil_Prices <- latihan3$World_Oil_Prices
freqdata=12 #data bulanan
World_Oil_Prices <- ts(World_Oil_Prices,start=c(1996,1),freq=freqdata) 
ts.plot(World_Oil_Prices,xlim=c(start(World_Oil_Prices)[1],(end(World_Oil_Prices)[1]+(1+(n.ahead.predict/frequency(World_Oil_Prices))))),
        ylab = "Observed/Fitted", main = "ARIMA Fitted vs Actual Data")
#plot data fitting in-sample
lines(fit.data,col="red")
#plot hasil prediksi dengan model ARIMA(1,1,0)
lines(pred.data,col="blue")
lines(pred.data.low,col="red",lty=3)
lines(pred.data.up,col="red",lty=3)
limitDate=end(World_Oil_Prices)[1]+(end(World_Oil_Prices)[2]-1)/frequency(World_Oil_Prices)
abline(v=limitDate ,lty=4)

#automatic arima
ArimaModel.8 <-auto.arima(latihan3$World_Oil_Prices.Log)
summary(ArimaModel.8)



