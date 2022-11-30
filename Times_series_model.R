
# 
timesSeries <- ts(datosTS, start = c(1976, 1), frequency = 12)

# Dibujamos el grafico de la serie temporal inicial
plot(timesSeries)

# Como no se ve una real mierda, vamos hacer los correlogramas
{acf(timesSeries, lag.max = 46*12)
pacf(timesSeries, lag.max = 46*12)}

# Realizamos el logaritmo al ser no estacionaria en variancia
log_ts <- log(timesSeries)
plot(log_ts)
{acf(log_ts, lag.max = 46*12)
  pacf(log_ts, lag.max = 46*12)}

# Realizamos una diferencia regular (d=1) debido a la no estacionariedad en media en la parte regular
log_ts_d1 <- diff(log_ts)
plot(log_ts_d1)
{acf(log_ts_d1, lag.max = 46*12)
pacf(log_ts_d1, lag.max = 46*12)}

# 
{acf(log_ts_d1, lag.max = 12)
  pacf(log_ts_d1, lag.max = 12)}


# ARIMA(0, 1, 2)(0, 0, 2)_12
forecast::auto.arima(timesSeries)
arima12 <- arima(timesSeries, order = c(0, 1, 2), seasonal = c(0, 0, 2))

residuals.plot(arima12)



