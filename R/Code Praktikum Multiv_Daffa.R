#Ujian Praktikum
#Edit lagi nanti
Tabel3.9<-data.frame(Tabel_3_9)

a<-mean(Tabel3.9$d1)
b<-mean(Tabel3.9$d2)
c<-mean(Tabel3.9$d3)

dbar <- rbind(a,b,c)
dbar

Difference<-Tabel3.9[c(7:9)]

Sd<-cov(Difference)
Sd

Inv_Sd <-solve(Sd)
Inv_Sd

Tkuadrat <- nrow(Tabel3.9)*(t(dbar))%*%Inv_Sd%*%(dbar)
Tkuadrat
