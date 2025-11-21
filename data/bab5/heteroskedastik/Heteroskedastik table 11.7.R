setwd("C:\\data\\bab5\\heteroskedastik\\")
dir()
mileage <- read.table("GujaratiTable11.7.txt",header=TRUE, sep="", 
                na.strings="NA", dec=".", strip.white=TRUE)

lm1 <- lm(MPG ~ SP + HP + WT, data=mileage)
summary(lm1)
#breusch Pagan test
library(lmtest)
bptest(lm1, studentize=FALSE, data=mileage)
#koenker method, remove normality assumption of error
bptest(lm1, studentize=TRUE, data=mileage)
# White test, adding second formula containing square and interaction
bptest(MPG ~ SP + HP + WT, varformula = ~HP +I(HP ^2)+SP +I(SP ^2)+WT +I(WT 
  ^2)+HP *SP +HP*WT+SP*WT, studentize=FALSE, data=mileage)

#menyelsaikan problem heteroskedasticity
#============Newey-West HAC method
library(sandwich)
vcov(lm1)
vcovHC(lm1,type="HC1")
#comparing coefficients
coeftest(lm1)#koefisien OLS
 #koefisien dengan metode Newey West HC
 #Digunakan opsi HC1 sebagai metode default yang digunakan Eviews
coeftest(lm1,vcov=vcovHC(lm1,type="HC1"))


#coba WT sebagai weight 
lm2 <- lm(MPG ~ SP + HP + WT, data=mileage,weights=1/WT)
summary(lm2)
bptest(lm2, studentize=FALSE, data=mileage)

