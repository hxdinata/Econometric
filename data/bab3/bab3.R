#section 3.1 

setwd(“c:\\data\\bab3”)
 datakeuangan = read.table("data.txt", header = T)
 datakeuangan

 datakeuangan2=datakeuangan[,-1] #buang kolom pertama 
 datakeuangan2
 datakeuangan2=ts(datakeuangan2,start=c(1996,1),frequency=12)
 datakeuangan2
 class(datakeuangan2) # terlihat datakeuangan2 bertipe multiple time series
 start(datakeuangan2) #melihat waktu awal dari datakeuangan2
 end(datakeuangan2) #melihat waktu akhir dari datakeuangan2
 frequency(datakeuangan2) #melihat waktu frekuensi waktu datakeuangan2

 FXUSDollar=datakeuangan2[,1] #mengekstrak datakeuangan2 kolom ke 1
 FXUSDollar
 summary(FXUSDollar)
 FXUSDollaraftercrisis=window(FXUSDollar,start=c(2003,12))# data bulanan, start Des 2003
 FXUSDollaraftercrisis

 FXUSDollar2=datakeuangan[,2]
 FXUSDollar2

 FXUSDollar3=ts(FXUSDollar2,start=c(1970,2),freq=4) #data kuartalan
 FXUSDollar3
 FXUSDollar4= ts(FXUSDollar2,end=2008,freq=1) # data tahunan
 FXUSDollar4
 FXUSDollar5= ts(FXUSDollar2,start=1,freq=1) # data undated
 FXUSDollar5

 plot(FXUSDollar,ylab="Kurs USDollar-RP",main="Rata-rata Bulanan Kurs USDollar-Rp",col="blue")

#section 3.2

x =1:10 
m=length(x)
x[(1+s):m]-x[1:(m-s)]
FXUSDollarDiff = diff(FXUSDollar)
plot(FXUSDollarDiff, main="Hasil Differensi order pertama")
FXUSDollarDiffLog = diff(log(FXUSDollar))
plot(FXUSDollarDiffLog, main="Hasil Differensi order pertama dari Log data")

#section 3.3
library(RcmdrPlugin.Econometrics)

