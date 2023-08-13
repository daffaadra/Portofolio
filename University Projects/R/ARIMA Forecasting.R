library(TSA)
library(forecast)
library(tseries)
library(normtest)
library(ggplot2)
library(tidyr)
library(lubridate)

dfs <- ts(ispu_data$o3, start= c(2018,2), frequency = 365.25/7)
dffs <- ts(ispu_data$o3, start= c(2018,2))
head(dfs)

#Split data untuk test
df <- head(dfs,-5)
df_test <- tail(dfs,5)
dff <- head(dffs,-5)
dff_test <- tail(dffs,5)

plot(df, main="Tingkat O3 (Jan 2018 - Jan 2021)")
#A. Uji Stasioneritas
adf.test(df)
diff(df)
plot(diff(df), ylab='O3', main = 'Tingkat O3 setelah Differencing')
adf.test(diff(df)) #<p-val <0.05, maka H0 ditolak dan data stasioner
Box.test(diff(df), type = c("Ljung-Box"))
Box.test(df, type = c("Ljung-Box"))

#B. Spesifikasi Model
tsdisplay(diff(df))
acf(diff(dff))
pacf(diff(dff))
eacf(diff(dff), ar.max=15, ma.max=15)

#C. Membuat Model
model_011 = Arima(df, order=c(0,1,1), method='ML', include.constant = TRUE)
model1 = Arima(dff, order=c(0,1,1), method='ML', include.constant = TRUE)
model_111 = Arima(df, order=c(1,1,1), method='ML', include.constant = TRUE)
model_212 = Arima(df, order=c(2,1,2), method='ML', include.constant = TRUE)
cbind(model_011, model_111, model_212)

model_011
model_111
model_212 
#Model 1 memiliki AIC terkecil 
model1 = model_011


#D. Model Diagnostic
plot(residuals(model_011), main = 'Residual Model ARIMA(0,1,1)')
adf.test(residuals(model_011)) #p-val <0.05, maka H0 dtiolak dan resid normal

qqnorm(model_011$residuals, main = "Normal Q-Q Plot Data Residual Tingkat O3")
qqline(model_011$residuals)

shapiro.test(model_011$residuals)
jarque.bera.test(model_011$residuals)

acf(residuals(model1))
checkresiduals(model_011, lag=21) #p-val > 0.05 maka H0 gagal ditolak maka tidak ada autokorelasi antar residual

#E. Overfitting
model_012 = Arima(df, order=c(0,1,2), include.constant = TRUE)
cbind(model_011, model_012)
model_011
model_012


#F. Forecasting (Actual vs Forecast)

prediksi <- forecast(model_011, h=5)
plot(prediksi)

prediksi

df_test
fcast_excl <- cbind(df_test, prediksi$mean, prediksi$lower[,1],
                    prediksi$upper[,1], prediksi$lower[,2], prediksi$upper[,2])
colnames(fcast_excl)<-c("Actual", "Point ForecaSt", "Lower 80%", "Upper 80%",
                        "Lower 95%", "Upper 95%")

fcast_excl














