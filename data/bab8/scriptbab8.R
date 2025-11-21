setwd("c:/data/bab8/")
M2=read.table("M2 Trend.txt",header=TRUE, sep="\t", 
                na.strings="NA", dec=".", strip.white=TRUE)
M2=ts(M2,start=c(1993,1),freq=12)
waktu=ts(1:length(M2),start=c(1993,1),freq=12)

 #trend linear
 trendlinear=lm(M2~waktu)
 summary(trendlinear)
 #trend kuadratik
 trendkuadrat=lm(M2~waktu+I(waktu^2))
 summary(trendkuadrat)
 #trend kubik
 trendkubik=lm(M2~waktu+I(waktu^2)+I(waktu^3))
 summary(trendkubik)

 ts.plot(M2)
 lines(ts(fitted(trendlinear),start=c(1993,1),freq=12),lty=2,col="red")
 lines(ts(fitted(trendkuadrat),start=c(1993,1),freq=12), lty=3,col="blue")
 lines(ts(fitted(trendkubik),start=c(1993,1),freq=12), lty=4,col="brown")
 legend(1995,1600000,c("Data","Trend Linear", "Trend kuadrat","Trend Kubik"),lty=c(1,2,3,4),col=c("black","red","blue","brown"), merge=T, bty="n")

 detrendlinear=trendlinear$residual
 detrendkuadrat=trendkuadrat$residual
 detrendkubik=trendkubik$residual

 ts.plot(detrendlinear,main="Data hasil Detrend",lty=1)
 lines(detrendkuadrat,lty=2)
 lines(detrendkubik,lty=3)
 legend(25,240000,c("Trend Linear", "Trend kuadrat","Trend Kubik"),lty=c(1,2,3),merge=T,bty="n")

%=========== 6.3. Exponential smoothing
 setwd("c:/data/bab8/")
 spain <-  read.table("spain.txt",header=TRUE, sep="\t", 
               na.strings="NA", dec=".", strip.white=TRUE) 
 spain=ts(spain,start=c(1970,1),freq=12) 
 ts.plot(spain,col="blue",main="Time Series Plot")
 fit1 <- HoltWinters(spain,alpha=NULL,beta=NULL,gamma=NULL, 
        seasonal="additive")
 fit2 <- HoltWinters(spain,alpha=NULL,beta=NULL,gamma=NULL, 
        seasonal="multiplicative")
 fit1$SSE
 fit2$SSE
 fit1
 fit2
 fit1$fitted
 fit2$fitted
 pred1 <- predict(fit1,n.ahead=6,prediction.interval=TRUE) 
 plot(fit1,pred1)


