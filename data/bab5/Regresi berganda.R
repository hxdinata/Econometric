setwd("C:\\data\\bab5\\")
tentara <- read.table("regresiberganda.txt",header=TRUE, sep="", 
                na.strings="NA", dec=".", strip.white=TRUE)
out<-lm(Y~X2+X3+X4+X5,data=tentara)
#out1<-lm(Y~ .,data=tentara)
summary(out)

bestmodel <- step(out) #backward
summary(bestmodel)
#diagnostic plot
par(mfrow=c(2,2))
plot(bestmodel,which=c(1:4))

m1 <- lm(formula = Y ~ X2 + X3 + X4 + X5, data = tentara)
m2 <- lm(formula = Y ~ X2 + X4 + X5, data = tentara)
anova(m1,m2)

m3 <- lm(formula = Y ~  X4 + X5 , data = tentara)
anova(m1,m3)

