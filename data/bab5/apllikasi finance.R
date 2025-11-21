#Regresi linear: CAPM 
setwd("c:/data/data/bab5/")
#setwd("c:/data/bab5/")
pgasdata <- read.table("PGAS.txt",header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
#transform the data into log returns
attach(pgasdata)
ldpgas = log(PGAS)[2:length(PGAS)]-log(PGAS)[1:(length(PGAS)-1)]
ldihsg = log(IHSG)[2:length(IHSG)]-log(IHSG)[1:(length(IHSG)-1)]
ldsbi = log(SBI)[2:length(SBI)]-log(SBI)[1:(length(SBI)-1)]
detach(pgasdata)
y=(ldpgas-ldsbi)
x=(ldihsg-ldsbi)
 #estimating beta of CAPM using regression
capm.1 <- lm( y~ x-1)
summary(capm.1 )
#estimating beta using covariance
cov(ldpgas,ldihsg)/var(ldihsg)

#===============================================================
#Regresi berganda
setwd("c:/data/data/bab5/")
#setwd("c:/data/bab5/")
SAHAM <- read.table("SAHAM.txt",header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

price1 = lm(price ~pe+eps+roi+roe+bv, data=SAHAM)
summary(price1)
#roe is insignificant and has the highest p-value
price2 = lm(price ~pe+eps+roi+bv, data=SAHAM)
summary(price2)
#eps is insignificant and has the highest p-value
price3 = lm(price ~pe+roi+bv, data=SAHAM)
summary(price3)
#pe is insignificant 
price4 = lm(price ~roi+bv, data=SAHAM)
summary(price4)
