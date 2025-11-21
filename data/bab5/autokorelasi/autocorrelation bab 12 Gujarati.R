setwd("C:\\data\\bab5\\autokorelasi\\")
dir()
wagesproductiv <- read.table("GujaratiTable12.4.txt",header=TRUE, sep="\t", 
                na.strings="NA", dec=".", strip.white=TRUE)

#======= regression and data plot
plot(wagesproductiv$X,wagesproductiv$Y)
abline(lm(wagesproductiv$Y~wagesproductiv$X),col="blue")

win.graph()
plot(log(wagesproductiv$X),log(wagesproductiv$Y))
abline(lm(log(wagesproductiv$Y)~log(wagesproductiv$X)),col="blue")

regwageproductiv=lm(Y~X,data=wagesproductiv)
summary(regwageproductiv)

reglogwageproductiv=lm(log(Y)~log(X),data=wagesproductiv)
summary(reglogwageproductiv)

#======= autocorrelation test
library(lmtest)
dwtest(regwageproductiv)
bgtest(regwageproductiv,order=6)
bgtest(regwageproductiv,order=1)

# ======== menyelesaikan problem autocorrelation 
# ======== two step Durbin Watson D Stat
dwstat=dwtest(regwageproductiv)$statistic
rho=1-(dwstat/2)
n=length(wagesproductiv$Y)
Xstar=wagesproductiv$X[2:n]-rho*wagesproductiv$X[1:(n-1)]
Ystar=wagesproductiv$Y[2:n]-rho*wagesproductiv$Y[1:(n-1)]
regstarwageproductiv=lm(Ystar~Xstar)
summary(regstarwageproductiv)
dwtest(regstarwageproductiv)
bgtest(regstarwageproductiv,order=6)

#============Newey-West HAC method
library(sandwich)
vcov(regwageproductiv)
vcovHAC(regwageproductiv)
#comparing coefficients
coeftest(regwageproductiv,vcov=vcovHAC)
coeftest(regwageproductiv)
coeftest(regstarwageproductiv)

