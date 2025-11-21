
setwd("c:\\data\\bab7")
nilai=read.table("logitnilai.txt",header=TRUE, sep="\t", 
                na.strings="NA", dec=".", strip.white=TRUE)
attach(nilai)

logit1 <- glm(Nilai~IPK+M1+M2,data=nilai, family=binomial(link="logit"))
summary(logit1)

logit2 <- glm(Nilai~IPK+M2,data=nilai, family=binomial(link="logit"))
summary(logit2)

#Odds ratio
thetahat <- logit2$fitted.values
odds_ratio <- logit2$fitted.values/(1-logit2$fitted.values)
cbind(Nilai,Fitted=round(thetahat,3),Odds=round(odds_ratio,3))

#Goodness of fit
logit20 <- glm(Nilai~1,data=nilai, family=binomial(link="logit")) #constanta model
1-as.vector(logLik(logit2)/logLik(logit20)) # pseudo R2

#predicted table 
table(true=nilai$Nilai, pred=round(fitted(logit2)))

#=========================================================

probit1 <- glm(Nilai~IPK+M1+M2,data=nilai, family=binomial(link="probit"))
summary(probit1)

probit2 <- glm(Nilai~IPK+M2,data=nilai, family=binomial(link="probit"))
summary(probit2)
