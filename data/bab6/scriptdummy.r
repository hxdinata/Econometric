#library(xlsReadWrite)
setwd("c:\\data\\bab6dummy") 
dir()
Dataset <- read.table("uangkeluar.txt",header=TRUE, sep="\t", 
                na.strings="NA", dec=".", strip.white=TRUE)
#Dataset <- read.xls("uangkeluar.xls")  
Dataset #menampilkan hasil

model.matrix(Y~X3,data=Dataset)
model.matrix(Y~X3-1,data=Dataset)
out1 <- lm(Y~X3,data=Dataset)
summary(out1)
out2 <-lm(Y~X3-1,data=Dataset)
summary(out2)

Dataset$X2 <- factor(Dataset$X2, levels = 0:1, labels = c("pria","wanita"))
summary(lm(Y~X1+X2+X3,data=Dataset))
summary(lm(Y~X2+X3,data=Dataset))
summary(lm(Y~X3,data=Dataset))