setwd("C:\\data\\bab12\\")
dataado <- read.table("ado.txt",header=TRUE, sep="", 
                na.strings="NA", dec=".", strip.white=TRUE)
dataado
dataado$ADO_I <- ts(dataado$ADO_I,start=1990,freq=1)
ts.plot(dataado$ADO_I,main="ADO sektor Industri")

library(tseries)
adf.test(dataado$ADO_I)
adf.test(diff(dataado$ADO_I))
dADO_I=diff(dataado$ADO_I)
acf(dADO_I)

ts.plot(dataado$GDP_I,main="GDP")
dataado$dGDP_I=c("NA",diff(dataado$GDP_I))

#ts.plot(dataado$INDEX,main="INDEX")
#adf.test(dataado$INDEX)
#dataado$dINDEX=c("NA",diff(dataado$INDEX))

library(TSA)
i_ado_m.1 <- arimax(dataado$ADO_I,order=c(1,1,0),seasonal=list(order=c(0,0,0),period=NA),
           include.mean=TRUE,xreg=data.frame(dataado[,c("dGDP_I","INDEX")]),method="CSS")
printstatarima(i_ado_m.1)

i_ado_m.2 <- arimax(dataado$ADO_I,order=c(0,1,0),seasonal=list(order=c(0,0,0),period=NA),
           include.mean=TRUE,xreg=data.frame(dataado[,c("dGDP_I","INDEX")]),method="CSS")
printstatarima(i_ado_m.2)

i_ado_m.3 <- arimax(dataado$ADO_I,order=c(0,1,0),seasonal=list(order=c(0,0,0),period=NA),
           include.mean=TRUE,xreg=data.frame(dataado[,c("dGDP_I")]),method="CSS")
printstatarima(i_ado_m.3)

#prediksi
n=length(dataado$ADO_I)
beta=as.numeric(coef(i_ado_m.3))
newGDP_I=139000
newdeltaGDP_I=newGDP_I-dataado$GDP_I[n]
#newOil=60
#newFX=9500
#newInfl=120
#newINDEX=(newOil/newFX)*newInfl
#predADO=dataado$ADO_I[n]+beta[1]*newdeltaGDP_I+beta[2]*newINDEX
predADO=dataado$ADO_I[n]+beta[1]*newdeltaGDP_I
predADO

