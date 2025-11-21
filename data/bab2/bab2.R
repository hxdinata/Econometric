#Bagian 1
data1= scan()

# ===================
datatxt = read.table("c:\\temp\\data.txt", header = T)
datatxt
# ===================
setwd("c:\\temp") #memindahkan direktori kerja ke c:\temp
dir() #melihat daftar file di direktori c:/temp 
datatxt2 = read.table("data.txt", header = T)
datatxt2 #menampilkan hasil
# ===================
datacsv= read.csv("c:\\temp\\data.csv", strip.white = TRUE)
datacsv
# ===================
library(xlsReadWrite)
setwd(“c:\\temp”) #memindahkan direktori kerja ke c:\temp
dataxls=read.xls("data.xls",dateTimeAs="isodate") 
#baca file data.xls dengan tidak merubah format variable Date 
dataxls #menampilkan hasil
# ===================
FXUSDollar = scan() #blok satu kolom data FXUSDollar

# ===================
setwd(“c:\\temp”)
library(foreign)
dataspss=read.spss("data.sav", use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
dataspss #menampilkan hasil import

# ===================
dataspss2=read.spss("data.sav",use.value.labels=FALSE)
dataspss2 #menampilkan hasil import

datatxt = edit(datatxt)
datatxt

# ===================
fix(datatxt)

# ===================
search()
objects(8) #sesuaikan dengan  nomor objek datasets di komp anda
AirPassengers
data(package = "datasets") 
data(package = "car") 
data(package = .packages(all.available = TRUE))

# ===================
write.table(x, file = "foo.csv", sep = ",", col.names = NA)
write.table(x, file = "foo.txt", sep = "\t", col.names = NA)

# ===================
setwd(“c://temp”)
datatxt = read.table("c:\\temp\\data.txt", header = T)
write.table(datatxt,file="dataexport.txt",sep="\t")
