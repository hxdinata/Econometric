 ## Simulate VAR(2)-data
 library(dse1)
 library(vars)
 ## Setting the lag-polynomial A(L) 
 Apoly   <- array(c(1.0, -0.5, 0.3, 0,
                   0.2, 0.1, 0, -0.2,
                   0.7, 1, 0.5, -0.3) ,
                 c(3, 2, 2))
 B <- diag(2)
 TRD <- c(5, 10)
 ## Generating the VAR(2) model 
 var2  <- ARMA(A = Apoly, B = B, TREND = TRD)
 ## Simulating 500 observations
 varsim <- simulate(var2, sampleT = 500, noise = list(w =   
     matrix(rnorm(1000),nrow = 500, ncol = 2)), rng = list(seed =  
     c(123456))) 
 ## Obtaining the generated series
 vardat <- matrix(varsim$output, nrow = 500, ncol = 2)
 colnames(vardat) <- c("y1", "y2")
 ## Plotting the series
 plot.ts(vardat, main = "", xlab = "")

 ## Determining an appropriate lag-order
 infocrit <- VARselect(vardat, lag.max = 3,type = "const")
 infocrit # order sesuai bentuk model simulasi yakni p=2
 ## Estimating the model
 varsimest <- VAR(vardat, p = 2, type = "const", season = NULL, exogen = NULL)
 ## Alternatively, selection according to SBC
 varsimest <- VAR(vardat, type = "const", lag.max = 3, ic = "SC")
 ## testing serial correlation
 args(serial.test) #melihat argumen dari fungsi serial.test
 ## Portmanteau-Test sampai lag ke h=16
 var2c.serial <- serial.test(varsimest, lags.pt = 16,
                            type = "PT.asymptotic")
 var2c.serial
 plot(var2c.serial, names = "y1")
 plot(var2c.serial, names = "y2")
## Forecasting objects of class varest
 predictions <- predict(varsimest, n.ahead = 25, ci = 0.95)
 ## Plot of predictions for y1
 plot(predictions, names = "y1")
 plot(predictions, names = "y2") 

# ====================================================
#membaca data 
setwd("c:\\data\\bab15")
datastock = read.table("stockpab.txt", header = T)

# data mengandung unit root tetapi difference tidak
plot.ts(datastock[,4])
plot.ts(datastock[,5])
datadiff=datastock[-1,c(4,5)]
VARselect(datadiff, lag.max = 4,type = "both") #nilai lag optimal 2
#estimasi dilakukan dengan menggunakan 
#varest <- VAR(datadiff, type = "both", lag.max = 4, ic = "AIC")
varest <- VAR(datadiff, type = "both", p=4)
summary(varest)
causality(varest,cause="LogChangeStockA")
causality(varest,cause="LogChangeStockB")
varest <- VAR(datadiff, type = "both", p=1)
summary(varest)

library(lmtest)
grangertest(LogChangeStockA~LogChangeStockB,data=datadiff,order = 4)
grangertest(LogChangeStockB~LogChangeStockA,data=datadiff,order =4)
#--------------------------------------------------------------------------
library(vars)

#membaca data dan konversi ke runtun waktu data bulanan
setwd("c:\\data\\bab15")
datavar = read.table("var.txt", header = T)
datavar = ts(datavar, start=c(1947,12),frequency=12)
datavarest=datavar[,-c(1,2)]

VARselect(datavarest, lag.max = 3,type = "const") #nilai lag optimal 2

#estimasi dilakukan dengan menggunakan p=1,seperti pada Koop (2006)
varest <- VAR(datavarest, p = 1, type = "const", season = NULL, exogen = NULL)
summary(varest)

#===========================================================================
#file 4.3.R dari Pfaff (2008)
library(urca)
set.seed(12345)
e1 <- rnorm(250, 0, 0.5)
e2 <- rnorm(250, 0, 0.5)
e3 <- rnorm(250, 0, 0.5)
u1.ar1 <- arima.sim(model = list(ar = 0.75),
                    innov = e1, n = 250)
u2.ar1 <- arima.sim(model = list(ar = 0.3),
                    innov = e2, n = 250)
y3 <- cumsum(e3)
y1 <- 0.8 * y3 + u1.ar1
y2 <- -0.3 * y3 + u2.ar1
y.mat <- data.frame(y1, y2, y3)

vecmout <- ca.jo(y.mat)
jo.results <- summary(vecmout)
jo.results

vecm.r2 <- cajorls(vecmout, r = 2)



