library(tseries)
library(urca)

#membaca data dan konversi ke runtun waktu data kuartal
setwd("c:\\data\\bab14")
datakonsumsi = read.table("konsumsi.txt", header = T)
datakonsumsi = ts(datakonsumsi, start=c(1966,4),frequency=4)

#langkah 1. uji unit root, tanpa komponen trend
#ts.plot(lc)
#ts.plot(li)
#ts.plot(lw)

ur.lc <- ur.df(lc, lags=1, type='none')
summary(ur.lc)
ur.li <- ur.df(li, lags=1, type='none')
summary(ur.li)
ur.lw <- ur.df(lw, lags=1, type='none')
summary(ur.lw)

#uji dua langkah Engle Granger
#langkah 2. regresi dan menyimpan residual
lc.eq <- summary(lm(lc ~ li + lw, data=datakonsumsi))
li.eq <- summary(lm(li ~ lc + lw, data=datakonsumsi))
lw.eq <- summary(lm(lw ~ li + lc, data=datakonsumsi))

error.lc <- ts(resid(lc.eq), start=c(1966,4),frequency=4)
error.li <- ts(resid(li.eq), start=c(1966,4),frequency=4)
error.lw <- ts(resid(lw.eq), start=c(1966,4),frequency=4)

#langkah 3. uji unit root pada residual
ci.lc <- ur.df(error.lc, lags=1, type='none')
summary(ci.lc)
ci.li <- ur.df(error.li, lags=1, type='none')
summary(ci.li)
ci.lw <- ur.df(error.lw, lags=1, type='none')
summary(ci.lw)

#estimasi model ECM 

# langkah 1. Definisikan error (t-1) dari regresi
# error.lc, error.li, error.lw

# langkah 2. Estimasi model ECM
datakonsumsidiff= diff(datakonsumsi)
n.kons.diff=nrow(datakonsumsidiff)

#data diff(t) dan diff(t-1), dpt juga dibuat dengan fungsi embed, lag
diffdata=cbind(datakonsumsidiff[-n.kons.diff,],datakonsumsidiff[-1,])
colnames(diffdata)=c("lc.d","li.d","lw.d","lc.d1","li.d1","lw.d1")
diffdata = ts(diffdata, start=c(1967,2),frequency=4) 

#data error
error.lc1 = ts(error.lc[-c(1,2)],start=c(1967,2),frequency=4)
error.li1 = ts(error.li[-c(1,2)],start=c(1967,2),frequency=4)
error.lw1 = ts(error.lw[-c(1,2)],start=c(1967,2),frequency=4)

# model ecm untuk consumption, tanpa komponen trend
# d(lc)(t)=c0+lambda*error.lc(t-1)+psi1*d(li)(t)+psi2*d(lw)(t)+epsilon(t)
# lebih umum sampai order 1,
# d(lc)(t)=c0+lambda*error.lc(t-1)+psi1*d(lc)(t-1)+ psi2*d(li)(t)+psi3*d(li)(t-1)+psi4*d(lw)(t)+psi5*d(lw)(t-1)+epsilon(t)

ecm.eq1 <- lm(lc.d ~ error.lc1 + li.d + lw.d,data=diffdata)
summary(ecm.eq1)
ecm.eq2 <- lm(lc.d ~ error.lc1 + lc.d1 + li.d + li.d1 + lw.d+ lw.d1,data=diffdata)
summary(ecm.eq2)

# model ecm untuk income, tanpa komponen trend

ecm.eq3 <- lm(li.d ~ error.li1 + lc.d + lw.d,data=diffdata)
summary(ecm.eq3)
ecm.eq4 <- lm(li.d ~ error.li1 + li.d1 + lc.d + lc.d1 + lw.d+ lw.d1,data=diffdata)
summary(ecm.eq4)

# model ecm untuk wealth, tanpa komponen trend

ecm.eq5 <- lm(lw.d ~ error.lw1 + lc.d + li.d,data=diffdata)
summary(ecm.eq5)
ecm.eq6 <- lm(lw.d ~ error.lw1 + lw.d1 + lc.d + lc.d1 + li.d+ li.d1,data=diffdata)
summary(ecm.eq6)
