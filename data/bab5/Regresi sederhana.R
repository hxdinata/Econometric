setwd("C:\\data\\bab5\\")
datapengeluaran <- read.table("regresisederhana.txt",header=TRUE, sep="\t", 
                na.strings="NA", dec=".", strip.white=TRUE)

summary(datapengeluaran)
#plot data dan garis regresi
model1<- gaji~pengeluaran
plot(model1,data=datapengeluaran)
regres1 <- lm(gaji~pengeluaran,data=datapengeluaran)
abline(regres1,col="blue")

summary(regres1)
#anova Ftest untuk regresi sederhana
anova(regres1)
library(car)
Anova(regres1)

out<-summary(regres1)
names(out)
#str(out)
coefficients(out) 


#point dan interval
coef(regres1)
confint(regres1, level = 0.95) #default 95%

#prediksi
predict(regres1, newdata = data.frame(pengeluaran = c(3700,3800,3900)),interval = "confidence")

#plot interval fitted regression
datapengeluaranseq=seq(from = 2200, to = 8400, by = 200)
gajipred <- predict(regres1, newdata = data.frame(pengeluaran = datapengeluaranseq),interval = "prediction")
plot(gaji~pengeluaran,data=datapengeluaran)
lines(gajipred[,1]~datapengeluaranseq,col="blue")
lines(gajipred[,2]~datapengeluaranseq,col="blue",lty=2)
lines(gajipred[,3]~datapengeluaranseq,col="blue",lty=2)

plot(regres1,which=2)

#testing uji wald dengan uji F
library(car)
linear.hypothesis(regres1, "pengeluaran = 3")



