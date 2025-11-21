setwd("c:/data/data/bab5/")
pgasdata <- 
  read.table("PGAS.txt",header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

attach(pgasdata)
ldpgas = log(PGAS)[2:length(PGAS)]-log(PGAS)[1:(length(PGAS)-1)]
ldihsg = log(IHSG)[2:length(IHSG)]-log(IHSG)[1:(length(IHSG)-1)]
ldsbi = log(SBI)[2:length(SBI)]-log(SBI)[1:(length(SBI)-1)]
detach(pgasdata)

y=(ldpgas-ldsbi)
x=(ldihsg-ldsbi)
capm.1 <- lm( y~ x-1)
summary(capm.1 )

cov(ldpgas,ldihsg)/var(ldihsg)

