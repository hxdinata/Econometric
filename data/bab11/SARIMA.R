# Persiapan data
 setwd("c:/data/bab11/")
 spain <-  read.table("spain.txt",header=TRUE, sep="\t", 
               na.strings="NA", dec=".", strip.white=TRUE) 
 spain #menampilkan data spain
 spain=ts(spain,start=c(1970,1),freq=12) # data bulanan 
 ts.plot(spain,col="blue",main="Time Series Plot")
 dslogspain=diff(log(spain),lag=12,difference=1) 
 ts.plot(dslogspain,col="blue",main="Time Series Plot")
 dlogspain=diff(log(spain),difference=1) #lag default=1
 ts.plot(dlogspain,col="red",main="Time Series Plot")
 lines(dslogspain,col="blue")
#Plot ACF/PACF
 par(mfrow=c(1,2))
 acf(spain,lag.max=36)
 pacf(spain,lag.max=36)
 par(mfrow=c(1,2))
 acf(dslogspain,lag.max=36)
 pacf(dslogspain,lag.max=36)
 spain=ts(spain,start=c(1970,1),freq=12)
 dslogspain=diff(log(spain),lag=12,difference=1) 
 ddslogspain=diff(dslogspain,lag=1,difference=1)
 par(mfrow=c(1,2))
 acf(ddslogspain,lag.max=36)
 pacf(ddslogspain,lag.max=36)
#Estimasi model
 library(forecast)
 model1<-Arima(ddslogspain, order = c(0, 0, 1), seasonal = list(order = c(1, 0, 0), period = 12),include.mean = F)
 summary(model1)
 printstatarima(model1)

 logspain=log(spain)
 model1.3<-Arima(logspain, order = c(0, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12),include.mean = F)
 summary(model1.3)
 printstatarima(model1.3)
 model2.1<-Arima(ddslogspain, order = c(0, 0, 1), seasonal = list(order = c(0, 0, 1), period = 12),include.mean = F)
 summary(model2.1)
 printstatarima(model2.1)
 logspain=log(spain)
 model2.2<-Arima(logspain, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12),include.mean = F)
 summary(model2.2)
 printstatarima(model2.2)
 library(tseries)
 model3 <- arma(ddslogspain, lag=list(ar=NULL,ma=c(1,12)), include.intercept=F)
 summary(model3)
#Diagnostic check
setwd("c:/data/bab10")
source("C:\\data\\bab11\\lampiran.R")
 acfStat(model1.2$residual,lag=20)
 tsdiag(model1.2)

#Forecast dengan model terbaik
 logspain=log(spain)
 model2.2<-Arima(logspain, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12),include.mean = F)
 logspain.fitted=fitted(model2.2)
 spain.fitted=exp(logspain.fitted)
 logspain.pred= predict(model2.2,n.ahead=12)#prediksi 12 langkah 
 logspain.low = logspain.pred$pred - 1.96 * logspain.pred$se
 logspain.up = logspain.pred$pred + 1.96 * logspain.pred$sevvv
 ts.plot(spain,ylab="Observed/Fitted",xlim=c(start(spain)[1], (end(spain)[1]+1.5)), ylim=c(min(spain),max(exp(logspain.up))), main="SARIMA Fitted vs Actual Data")
 lines(spain.fitted,col="blue")
 lines(exp(logspain.pred$pred),col="red")
 lines(exp(logspain.low),col="red",lty=4)
 lines(exp(logspain.up),col="red",lty=4)
 limitDate=end(spain)[1]+(end(spain)[2]-1)/frequency(spain)
 abline(v=limitDate,lty=4)



