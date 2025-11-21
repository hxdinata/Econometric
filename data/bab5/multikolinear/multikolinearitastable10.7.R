setwd("C:\\data\\bab5\\multikolinear\\")
#dir()
longley <- read.table("Longley data.txt",header=TRUE, sep="", 
                na.strings="NA", dec=".", strip.white=TRUE)
#longley <- longley[,c(-1)]
regres1 <- lm(Y ~ X1 + X2 + X3 +X4 +X5 +TIME, data=longley)
summary(regres1)

#multikolinearitas check
cor(longley[,c(-1,-2)])

# regression by dropping last data
longley2=longley[c(-16),]
regres1a <- lm(Y ~ X1 + X2 + X3 +X4 +X5 +TIME, data=longley2)
summary(regres1a)

#check VIF
vif(regres1)

#aux reg, check only R^2
summary(lm(Y ~ X1 + X2 + X3 +X4 +X5 +TIME, data=longley))
summary(lm(X1 ~ X2 + X3 +X4 +X5 +TIME, data=longley))
summary(lm(X2 ~ X1 + X3 +X4 +X5 +TIME, data=longley))
summary(lm(X3 ~ X1 + X2 +X4 +X5 +TIME, data=longley))
summary(lm(X4 ~ X1 + X2 +X3 +X5 +TIME, data=longley))
summary(lm(X5 ~ X1 + X2 +X3 +X4 +TIME, data=longley))
summary(lm(TIME ~ X2 + X3 +X4 +X5 , data=longley))


#solving multicollinearity
longley$RGNP=longley$X2/longley$X1
regres2 <- lm(Y ~ RGNP + X4 +X5 , data=longley)
summary(regres2)
vif(regres2)



