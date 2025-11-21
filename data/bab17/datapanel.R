library(plm)
setwd("c:\\data\\bab17")
datapanel=read.table("datapanel.txt",header=TRUE, sep="\t", 
                na.strings="NA", dec=".", strip.white=TRUE)
# format data adalah stacked data frame menurut "cross section" dengan dua kolom pertama utk "cross section" dan "time"
#Uji Hausmann
#model1 
gf=NULL;gr=NULL
gf=plm(BANTUAN~PAD+SDO+YCAP+POP+BHPBP,data=datapanel,model="within")
gr=plm(BANTUAN~PAD+SDO+YCAP+POP+BHPBP,data=datapanel,model="random")
phtest(gf,gr)
#model2 
gf=NULL;gr=NULL
gf=plm(BANTUAN~PAD+SDO+YCAP+POP,data=datapanel,model="within")
gr=plm(BANTUAN~PAD+SDO+YCAP+POP,data=datapanel,model="random")
phtest(gf,gr)
#model3 
gf=NULL;gr=NULL
gf=plm(BANTUAN~PAD+SDO+POP,data=datapanel,model="within")
gr=plm(BANTUAN~PAD+SDO+POP,data=datapanel,model="random")
phtest(gf,gr)
#model4 
gf=NULL;gr=NULL
gf=plm(BANTUAN~PAD+SDO,data=datapanel,model="within")
gr=plm(BANTUAN~PAD+SDO,data=datapanel,model="random")
phtest(gf,gr)

#Uji Breusch Pagan 
#model1 
g=NULL
g=plm(BANTUAN~PAD+SDO+YCAP+POP+BHPBP,data=datapanel,model="pooling")
plmtest(g,effect="twoways",type="bp")
plmtest(g,effect="individual",type="bp")
plmtest(g,effect="time",type="bp")

#model2 
g=NULL
g=plm(BANTUAN~PAD+SDO+YCAP+POP,data=datapanel,model="pooling")
plmtest(g,effect="twoways",type="bp")
plmtest(g,effect="individual",type="bp")
plmtest(g,effect="time",type="bp")

#model3 
g=NULL
g=plm(BANTUAN~PAD+SDO+POP,data=datapanel,model="pooling")
plmtest(g,effect="twoways",type="bp")
plmtest(g,effect="individual",type="bp")
plmtest(g,effect="time",type="bp")

#model4 
g=NULL
g=plm(BANTUAN~PAD+SDO,data=datapanel,model="pooling")
plmtest(g,effect="twoways",type="bp")
plmtest(g,effect="individual",type="bp")
plmtest(g,effect="time",type="bp")

#estimasi model
# Model I: Model Efek tetap, dengan efek waktu
g=NULL
g=plm(BANTUAN~PAD+SDO+YCAP+POP+BHPBP,data=datapanel,model="within",effect="time")
summary(g)
fixef(g,type="level") #efek waktu untuk setiap level waktu
#fixef(g,type="dmean") #efek waktu dihitung dari rata-rata seluruh waktu

# Ia. PAD tidak signifikan
g=plm(BANTUAN~SDO+YCAP+POP+BHPBP,data=datapanel,model="within",effect="time")
summary(g)
# 1b. YCAP tidak signifikan
g=plm(BANTUAN~SDO+POP+BHPBP,data=datapanel,model="within",effect="time")
summary(g)
# 1c.BHPBP tidak signifikan
g=plm(BANTUAN~SDO+POP,data=datapanel,model="within",effect="time")
summary(g)
fixef(g,type="level")


#model2 Model Efek tetap, regresi pooling
g=NULL
g=plm(BANTUAN~PAD+SDO+YCAP+POP,data=datapanel,model="pooling")
summary(g)
#variabel YCAP tidak signifikan, dapat dihilangkan dari model, menjadi model 3
#model3 Model Efek tetap, regresi pooling
g=plm(BANTUAN~PAD+SDO+POP,data=datapanel,model="pooling")
summary(g)

#model4 Model Efek tetap, dengan efek individu 
g=NULL
g=plm(BANTUAN~PAD+SDO,data=datapanel,model="within",effect="individual")
summary(g)
fixef(g,type="dmean") #efek waktu dihitung dari rata-rata


#===================================
#Uji korelasi serial
g1c=plm(BANTUAN~SDO+POP,data=datapanel,model="within",effect="time")
pbgtest(g1c, order=2)

g3=plm(BANTUAN~PAD+SDO+POP,data=datapanel,model="pooling")
pbgtest(g3, order=2)

g4=plm(BANTUAN~PAD+SDO,data=datapanel,model="within",effect="individual")
pbgtest(g4, order=2)


#==========================
# Heteroscedasticity Robust Covariance Estimator

library(lmtest)

g3=plm(BANTUAN~PAD+SDO+POP,data=datapanel,model="pooling")
coeftest(g3,vcovHC)
summary(g3)

g4=plm(BANTUAN~PAD+SDO,data=datapanel,model="within",effect="individual")
coeftest(g4,vcovHC)
summary(g4)

