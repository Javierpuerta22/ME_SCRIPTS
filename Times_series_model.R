
# Convertim la sèrie temporal en un objecte temporal
timesSeries <- ts(datosTS, start = c(1976, 1), frequency = 12)

# Dibuixem el gràfic de la sèrie inicial
plot(timesSeries)

# Fem els correlogrames ja que no es veu molta cosa
{acf(timesSeries, lag.max = 46*12)
pacf(timesSeries, lag.max = 46*12)}
#Las FAS(ACF) decreix lentament, per tant la sèrie no és estacionària
# Realitzem el logaritme al no ser estacionària en variança
#Com la sèrie no és estacionària, hem de fer el logaritme i al veure la Fas i la FAP
# del logaritme, podem veure si es AR o MA
log_ts <- log(timesSeries)
plot(log_ts)
{acf(log_ts, lag.max = 46*12)
  pacf(log_ts, lag.max = 46*12)}

#La mitja dels valors al llarg del temps de la sèrie temporal no és constant, per tant, no és estacionària.
# Realitzem una diferència regular (d=1) a causa de la no estacionaritat en mitja
log_ts_d1 <- diff(log_ts)
plot(log_ts_d1)
{acf(log_ts_d1, lag.max = 46*12)
pacf(log_ts_d1, lag.max = 46*12)}

# 
{acf(log_ts_d1, lag.max = 12)
  pacf(log_ts_d1, lag.max = 12)}


# Model final ARIMA(0, 1, 2)(0, 0, 2)_12
library(forecast)
forecast::auto.arima(timesSeries)
arima12 <- arima(timesSeries, order = c(0, 1, 2), seasonal = c(0, 0, 2))
summary(arima12)
library(lmtest)
coeftest(arima12)

#Validació del model
residuales <- residuals(arima12)
mean(residuales)
var(residuales)
checkresiduals(arima12)

# Realitzem les prediccions amb un 60% de les mostres i veiem els resultats
a <- length(timesSeries) * 0.6
setenta <- datosTS[0:a]
timeSeries70 <- ts(setenta,start=c(1976,1), frequency = 12)
arima70 <- arima(timeSeries70, order = c(0, 1, 2), seasonal = c(0, 0, 2))
pred <- forecast(arima70,h=48)
b <- a+47
aux <-datosTS[a:b]
autoplot(pred)
accuracy(pred,aux)

