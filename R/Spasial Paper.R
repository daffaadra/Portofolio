#Import Library
library(maptools)
library(sp)
library(foreign)
library(lattice)
library(rgdal)
library(RColorBrewer)
library(proxy)
library(classInt)
library(class)
library(e1071)
library(shapefiles)
library(spdep)
library(raster)
library(Matrix)
library(tidyverse)
library(lmtest)
library(spatialreg)
library(DescTools)



#MODEL OLS
data <- house_prices_new_
sum(is.na(data))
na.omit(data)

y = data$Price
x1 = data$Baths
x2 = data$Beds
x3 = data$`Land size`
x4 = data$`House size`

library(car)
model1<- lm(y~x1+x2+x3+x4)
summary(model1)
AIC(model1)

size_sum(data)

#UJI ASUMSI
#Ho : error dist normal
shapiro.test(residuals(model1))
library(nortest)
ad.test(residuals(model1))
qqnorm(residuals(model1),datax=T)
#Ho : galat model saling bebas
durbinWatsonTest(model1)
#Ho : ragam galat homogen
library(lmtest)
bptest(model1, studentize=F, data=data)
#Ho : model linier
resettest(model1)
#Multikolinieritas
vif(model1)

#UJI MORANS'I
peta<-rgdal::readOGR("/Users/daffa/Downloads/SHP_Jabar/kabkot_jawabarat.shp")
rgdal::ogrListLayers("/Users/daffa/Downloads/SHP_Jabar/kabkot_jawabarat.shp")

petanew<-peta[-c(28:28),]

peta1<-rgdal::readOGR("/Users/daffa/Downloads/lka_adm_slsd_20200305_shp/lka_admbnda_adm0_slsd_20200305.shp",)
rgdal::ogrListLayers("/Users/daffa/Downloads/lka_adm_slsd_20200305_shp/lka_admbnda_adm0_slsd_20200305.shp")

names(peta1)
peta1
petanew

?poly2nb
w<-poly2nb(peta1,queen=TRUE)
w

plot(petanew, col="white", border="grey")
plot(w,coordinates(petanew), col="red", add=TRUE)
text(coordinates(petanew), labels=data$`kabupaten/kota`, 
     cex=0.4, col="blue",pos=3, offset=0.6)
ww<-nb2listw(w)
moran(data$TPT, ww, n=length(ww$neighbours), S0=Szero(ww))
moran.test(data$TPT, ww,randomisation=T, alternative="greater")
moran.test(data$`Jumlah Penduduk`, ww,randomisation=T, alternative="greater")
moran.test(data$`Jlh Pdd Mskn`, ww,randomisation=T, alternative="greater")
moran.test(data$TPAK, ww,randomisation=T, alternative="greater")
moran.test(data$IPM, ww,randomisation=T, alternative="greater")
moran.test(data$Jamkes, ww,randomisation=T, alternative="greater")

moran.plot(data$TPT, ww, labels=data$`kabupaten/kota`)

#UJI LAGRANGE
model1<-lm(y~x1+x2+x3+x4,data=data)
LM<-lm.LMtests(model1, nb2listw(w, style="W"),test=c("LMerr", "LMlag","RLMerr","RLMlag","SARMA"))
summary(LM)
