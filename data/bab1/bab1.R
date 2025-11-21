#section 1.7
library(Rcmdr)
Commander()
install.packages("Rcmdr", dep=T)

#section 1.8
help(plot)
?plot
example(plot)

library(help=foreign)
help(package=foreign)
library(foreign)
? read.spss

apropos("plot")
help.search("plot")
??plot

#section 1.9
save("latihan1", file="C:/temp/latihan1.rda")
load("C:/temp/latihan1.rda")