%======================= Dekomposisi dan seasonal adjustment dengan R-CLI

setwd("C:/data/bab9/")
spain <- read.table("spain.txt", header=TRUE, sep="\t", 
  na.strings="NA", dec=".", strip.white=TRUE)
spain=ts(spain, start=c(1970,1),freq=12)
spaindecompose=decompose(spain)
plot(spaindecompose)

spaindecompose$seasonal
spaindecompose$trend
spaindecompose$random

seasadj(spaindecompose) #menampilkan seasonal adjusted data
ts.plot(spain,smain="Time Series Plot")
lines(seasadj(spaindecompose),col="blue")

sindexf(spaindecompose,12) 
lines(sindexf(spaindecompose,12) )