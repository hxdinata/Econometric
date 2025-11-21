#simulasi ARCH.GARCH
library(fGarch)
# simulasi ARMA GARCH(1,1) dengan parameter default
# K=omega= 1e-06, alpha= 0.1, beta =0.8
# dapat dilihat dengan garchSpec()
spec = garchSpec(model = list(ar = 0.5, ma = c(0.3, -0.3)))  
arma12garch11=garchSim(spec, n = 1000)

## More simulations ...

# Default GARCH(1,1) - uses default parameter settings
spec = garchSpec(model = list())
garchSim(spec, n = 10)
   
# ARCH(2) - use default omega and specify alpha, set beta=0!
 spec = garchSpec(model = list(alpha = c(0.2, 0.4), beta = 0))
 garchSim(spec, n = 10)

# AR(1)-ARCH(2) - use default mu, omega
spec = garchSpec(model = list(ar = 0.5, alpha = c(0.3, 0.4), beta = 0))
garchSim(spec, n = 10)
   
# AR([1,5])-GARCH(1,1) - use default garch values and subset ar[.]
spec = garchSpec(model = list(mu = 0.001, ar = c(0.5,0,0,0,0.1)))
garchSim(spec, n = 10)

# GARCH(1,1) - use default omega and specify alpha/beta
spec = garchSpec(model = list(alpha = 0.2, beta = 0.7))
garchSim(spec, n = 10)
   
# GARCH(1,1) - specify omega/alpha/beta
spec = garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8))
garchSim(spec, n = 10)
   
# GARCH(1,2) - use default omega and specify alpha[1]/beta[2]
spec = garchSpec(model = list(alpha = 0.1, beta = c(0.4, 0.4)))
garchSim(spec, n = 10)
   
# GARCH(2,1) - use default omega and specify alpha[2]/beta[1]
spec = garchSpec(model = list(alpha = c(0.12, 0.04), beta = 0.08))
garchSim(spec, n = 10)

# jalankan simulasi untuk mendapat kan data arma12garch11 diatas
spec = garchSpec(model = list(ar = 0.5, ma = c(0.3, -0.3)))  
arma12garch11=garchSim(spec, n = 1000)
# estimasi model ARMA(1,2)+Garch(1,1) dari hasil simulasi 
 fit <- garchFit(~arma(1,2)+garch(1,1),data= arma12garch11,trace=F, 
         cond.dist="QMLE",include.mean=F)
summary(fit) #lmarch test show no further garch effect

#Pemodelan data empiris
#import data
 library(fGarch)
 setwd("c:/data/bab16") #mengubah direktori kerja
 dir() #untuk melihat data yang ada dalam direktori c:/data/bab9
 DATA <- read.table("deugbp.txt",header=TRUE, sep="\t", 
              na.strings="NA", dec=".", strip.white=TRUE) 
 DATA #menampilkan data/objek deugbp
#identifikasi
 ts.plot(DATA$DEUGBP,col="blue",main="Time Series Plot")
 rdeugbp=diff(log(DATA$DEUGBP), differences = 1)
 ts.plot(rdeugbp,col="blue",main="Plot log return")
 par(mfrow=c(1,2))
 acf(rdeugbp)
 pacf(rdeugbp)
source("C:\\data\\bab16\\lampiran.R")
 acfStat(rdeugbp,36)
 par(mfrow=c(1,2))
 acf(rdeugbp^2)
 pacf(rdeugbp^2)
 acfStat(rdeugbp^2)
 ArimaModel.1 <- Arima(World_Oil_Prices.Log,order=c(1,1,0),
   seasonal=list(order=c(0,0,0),period=NA),include.mean=FALSE)
 residu=ArimaModel.1$residuals #menghitung residual model ArimaModel.1
acf(residu)
acf(residu^2)
#Estimasi model 
 library(tseries)
 fit=garch(rdeugbp, order = c(1, 2),trace=F)
 summary(fit) # tampilan output
 sum(fit$coef) # jumlahan koefisien hasil estimasi <1 
 plot(fit) # plot diagnostic check, terlihat residual non normal

 fit2 <- garchFit(~garch(2,1),data=rdeugbp, trace=F, algorithm = "lbfgsb+nm") 
 summary(fit2) 

 fit3 <- garchFit(~garch(2,1),data=rdeugbp, include.mean=F,trace=F, algorithm = "lbfgsb+nm") 
 summary(fit3) 

 fit4 <- garchFit(~garch(1,1),data=rdeugbp, include.mean=F,trace=F, algorithm = "lbfgsb+nm") 
 summary(fit4)

 fit4@fit$coef
plot(fit4) #menampilkan pilihan plot diagnostic check dari fit4
plot(fit4) #menampilkan pilihan plot diagnostic check dari fit4
 fit5 <- garchFit(~garch(1,1),data=rdeugbp, include.mean=F,trace=F, algorithm = "lbfgsb+nm",cond.dist="QMLE") 
 summary(fit5)
 fit6 <- garchFit(~garch(1,1),data=rdeugbp,include.mean=F, trace=F, algorithm = "lbfgsb+nm",cond.dist="std")#matrik cvar NaN 
 summary(fit6) # output menghasilkan SE NaN
#prediksi dengan model terbaik
 fit7=garchFit(~garch(1,2),data=rdeugbp, include.mean=F,trace=F, algorithm = "lbfgsb+nm",cond.dist="QMLE")
 predict(fit7,n.ahead=10) #prediksi 10 langkah nilai mean dan stddev
 predict(fit7,n.ahead=10,plot=TRUE,nx=100) #plot bersama 100 data
 predict(fit7,n.ahead=10,plot=TRUE,conf=.9,nx=100)#digunakan confidence level 90% untuk prediksi mean
 fit7@fit$series$h # nilai fitted dari variance
 sqrt(fit7@fit$series$h) # nilai fitted stddev/volatility

 fitted.vol=ts(sqrt(fit7@fit$series$h),freq=1)
 plot(fitted.vol,xlim=c(1,length(fitted.vol)+100),type="l"
		,ylab="Volatility",main="Volatility Plot")
 pred.vol=predict(fit7,n.ahead=100)$standardDeviation
 abline(v=length(fitted.vol),lty=4)
 lines(ts(pred.vol, freq=1,start=1975),col="red")


