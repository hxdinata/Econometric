#section 4.1
setwd("c:\\data\\bab4\\") 
latihan3 = read.table("data.txt", header = T)
latihan3
summary(latihan3)
library(fBasics) 
OilPrices=latihan3[,5] #Variabel OilPrices ada dikolom ke 5 
kurtosis(OilPrices) #Dihitung excess kurtosis=kurtosis -3
skewness(OilPrices) 
hist(OilPrices) 
plot(density(OilPrices),main="Estimasi densitas dari data")

#=========
hFX=hist(OilPrices)
xhist=c(min(hFX$breaks),hFX$breaks) #membentuk vektor interval
yhist=c(0,hFX$density,0) # membentuk vektor estimasi prob. utk interval
xfit=seq(min(OilPrices),max(OilPrices),length=40)
yfit=dnorm(xfit,mean=mean(OilPrices),sd=sd(OilPrices)) 
# nilai densitas distribusi normal untuk interval nilai di xfit
plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)),main="Normal pdf dan histogram") # plot histogram, dengan sumbu y nilai estimasi prob
lines(xfit,yfit) # menambahkan plot densitas kedalam grafik
#==========
qqnorm(OilPrices) )#plot  qqplot terhadap distribusi normal
qqline(OilPrices)#garis menghubungkan quartile 1 dan 3
library(car)
qq.plot(OilPrices, dist= "norm",main="Normal QQ Plot")

#section 4.2.
shapiro.test(OilPrices) 
library(tseries) #pastikan library tseries telah diinstal
jarque.bera.test(OilPrices)

library(nortest) #install terlebih dahulu dari cd instalasi
sf.test(OilPrices)
ad.test(OilPrices)
cvm.test(OilPrices)
lillie.test(OilPrices)
pearson.test(OilPrices)

#bab 4.3.
library(car) #pastikan library ini udah terinstal
box.cox.powers(OilPrices)
library(tseries) #pastikan library tseries telah diinstal
jarque.bera.test(OilPrices^-0.343) #power
jarque.bera.test(((OilPrices^-0.343) -1)/-0.343) #boxcox

#bab 4.4.

OilPrices=latihan3[,5] #Variabel OilPrices ada dikolom ke 5 
OilPrices=ts(OilPrices,start=c(1996,1),frequency=12) #mengubah data menjadi tipe time series
ts.plot(OilPrices, main="Monthly Oil Price per Barrel")
OilPrices=latihan3[,5] #Variabel OilPrices ada dikolom ke 5 
OilPrices=ts(OilPrices,start=c(1996,1),frequency=12) #mengubah data menjadi tipe time series
CPI=latihan3[,3] #Variabel CPI ada dikolom ke 3 
CPI=ts(CPI,start=c(1996,1),frequency=12) # mengubah menjadi tipe time series

#=============
win.graph() #membuka jendela grafik baru
ts.plot(OilPrices,CPI,gpars=list(xlab="year", ylab="", lty=c(1,2)), main="Multiple plot") #contoh multiple plot
legend(1996, 150, c("Oil Prices","CPI"), bty="n",lty = c(1, 2), merge = TRUE) #menambahkan legenda dari plot
#=============
win.graph()
par(mfrow=c(3,1)) #membagi jendela grafik menjadi 3 baris, 1 kolom > OilPrices=latihan3[,5]
ts.plot(OilPrices, main="Monthly Oil Price per Barrel")
acf(OilPrices) #plot fungsi ACF
pacf(OilPrices) #plot fungsi PACF
#=========
graph()
par(mfrow=c(3,1)) #membagi jendela grafik menjadi 3 baris, 1 kolom
OilPrices=latihan3[,5]
ts.plot(diff(OilPrices), main="Monthly Oil Price per Barrel")
acf(diff(OilPrices)) #plot fungsi ACF
pacf(diff(OilPrices)) #plot fungsi PACF

#=========
OilPrices=latihan3[,5]
ts.plot(OilPrices, main="Monthly Oil Price per Barrel")
library(tseries)
adf.test(OilPrices) #H0 diterima, data non stasioner
adf.test(diff(OilPrices)) #Ho ditolak, data difference stasioner

library(urca)
out1=ur.df(OilPrices,type="trend", lags=5)
summary(out1)

out2=ur.df(OilPrices,type="trend", lags=1)
summary(out2)

out3=ur.df(diff(OilPrices),type="trend", lags=5)
summary(out3)