library(dynlm)
library(urca)

setwd("c:\\data\\bab13")
liberal=read.table("liberal.txt",header=TRUE, sep="\t", 
                na.strings="NA", dec=".", strip.white=TRUE)

liberal=ts(liberal,start=c(1,1), freq=1)
#variabel pada liberal adalah time, deltaGDP, deltastock
#uji stasioneritas dari y dan x
summary(ur.df(liberal$deltaGDP, lags=1, type='none'))
summary(ur.df(liberal$deltastock, lags=1, type='none'))

#model ADL(2,2) untuk x dan y stasioner
# nontransformed ADL
# y(t)=alpha+delta*time+rho1*y(t-1)+ rho2*y(t-2)+theta0*x(t)+theta1*x(t-1)+omega2*x(t-2)+et
# ekuivalently, transformed ADL
# delta.y(t)= alpha+delta*time+rho*y(t-1)+gamma1*delta.y(t-1)+theta*x(t)+omega1*delta.x(t)+omega2*delta.x(t-1)+et
# default using transformed ADL
# y=deltaGDP, x= deltastock

#data harus bertipe time series 

adl22nontranf=dynlm(deltaGDP~time+L(deltaGDP,1:2)+deltastock+L(deltastock,1:2),data=liberal)
summary(adl22nontranf)

adl22transf= dynlm(d(deltaGDP)~time+L(deltaGDP)+L(d(deltaGDP),1:1)+deltastock+L(d(deltastock),0:1),data=liberal)
summary(adl22transf)