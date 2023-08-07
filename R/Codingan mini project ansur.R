attach(DataAnsur)
df<- DataAnsur

install.packages("survival")

library(survival)

St <- Surv(DataAnsur$Time, DataAnsur$Censor)
St
#Survival function tanpa kriteria
sf1<-survfit(Surv(df$Time,df$Censor)~1,data=df)
summary(sf1)

#plot sf1
plot(sf1,main='Fungsi survival tanpa kriteria',
     xlab = 'Waktu Survival (t)',
     ylab = 'Fungsi Survival S(t)',)

#Survival function dengan kriteria komorbiditas
sf2<-survfit(Surv(df$Time,df$Censor)~Comorb,data=df)
summary(sf2)

#plot sf2
plot(sf2,main='Fungsi survival berdasarkan Komorbiditas',
     col=c('red','blue','green'),
     xlab='Waktu Survival (t)',
     ylab='Fungsi Survival S(t)')
legend('topright',c('Type 0','Type 1','Type 2'),lty=1,col=c('red','blue','green'),bty='n',cex=0.4)

#Survival function dengan kriteria umur
df$AgeGroup<-ifelse(df$Age>=60,'Geriatri','Non-Geriatri')
sf3<-survfit(Surv(df$Time,df$Censor)~AgeGroup,data=df)
summary(sf3)

#plot sf3
plot(sf3,main='Fungsi Survival Kaplan-Meier berdasarkan Kelompok Usia',
     col=c('red','blue'),
     xlab='Waktu Survival (t)',
     ylab='Fungsi Survival S(t)')
legend('topright',c('Geriatri','Non Geriatri'),lty=1,
       col=c('red','blue'),bty='n',cex=0.4)

#Survival function dengan kriteria umur dan komorbiditas
sf4<-survfit(Surv(df$Time,df$Censor)~AgeGroup+Comorb,data=df)
summary(sf4)

#plot sf4
plot(sf4,main='Fungsi survival berdasarkan Komorbiditas dan Kel Usia',
     xlab='Waktu Survival (t)',
     col=c('turquoise4','turquoise','turquoise1','firebrick4','firebrick3','firebrick1'),
     ylab='Fungsi Survival S(t)')

#Fungsi hazard berdasarkan kel usia
stgeriatri<-Surv(df$Time[df$AgeGroup=='Geriatri'],df$Censor[df$AgeGroup=='Geriatri'])
sfgeriatri<-survfit(stgeriatri~1)
summary(sfgeriatri)
hfgeriatri<-sfgeriatri$n.event/sfgeriatri$n.risk
hfgeriatri

stnon<-Surv(df$Time[df$AgeGroup=='Non-Geriatri'],df$Censor[df$AgeGroup=='Non-Geriatri'])
sfnon<-survfit(stnon~1)
summary(sfnon)
hfnon<-sfnon$n.event/sfnon$n.risk

#Membuat table fungsi hazard
Waktu.Geriatri<-sfgeriatri$time[sfgeriatri$n.event>0]
HazardFunction.Geriatri<-hfgeriatri[hfgeriatri>0]
dfg<-data.frame(Waktu.Geriatri,HazardFunction.Geriatri)
View(dfg)

Waktu.NonGeriatri<-sfnon$time[sfnon$n.event>0]
HazardFunction.NonGeriatri<-hfnon[hfnon>0]
dfnon<-data.frame(Waktu.NonGeriatri,HazardFunction.NonGeriatri)
View(dfnon)

plot(dfg$Waktu.Geriatri,dfg$HazardFunction.Geriatri,
     main = 'Estimasi Fungsi Hazard Nelson-Aalen',
     xlab = 'Waktu Survival(t)',
     ylab='Fungsi Hazard h(t)')
lines(dfg$Waktu.Geriatri,dfg$HazardFunction.Geriatri)

plot(dfnon$Waktu.NonGeriatri,dfnon$HazardFunction.NonGeriatri,
     main = 'Estimasi Fungsi Hazard Nelson-Aalen',
     xlab = 'Waktu Survival(t)',
     ylab='Fungsi Hazard h(t)')
lines(dfnon$Waktu.NonGeriatri,dfnon$HazardFunction.NonGeriatri)
