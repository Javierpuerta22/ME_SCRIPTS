# Carreguem els paquets necesaris
install.packages("forecast")
library(forecast)
library(ggplot2)
library(readxl)
library(httr)
# library(ts)

# ==============================================================================
# Operacions a realitzar amb sÃ©ries temporals
## CombinaciÃ³n de series temporales
serie1 <- ts(1:20, freq=12, start=c(1981,3))
serie2 <- ts(1:15, freq=12, start=c(1980,9))

### junta las dos series, rellenando con NA los periodos en que una serie sea mÃ¡s larga que la otra.
ts.union(serie1,serie2)

### junta ambas series, pero sÃ³lo con los datos correspondientes al periodo comÃºn a ambas.
ts.intersect(serie1,serie2)

# ------------------------------------------------------------------------------
## Temporalidad
### Las funciones start() y end() muestran, respectivamente, los instantes correspondientes a la observaciones inicial y final de una serie temporal:
start(serie1)
end(serie1)

### La funciÃ³n window() extrae los valores de la serie temporal comprendidos entre una fecha de inicio y otra final. Podemos especificar sÃ³lo el aÃ±o:
window(serie1, start = c(1981, 5), end = c(1982, 6)) #del mes 5 al 6

### La funciÃ³n time() crea un objeto de clase ts con los instantes en que ha sido muestreada una serie temporal (en escala decimal)
time(serie1)

### Estas funciones nos muestran el tiempo entre observaciones (deltat()) y nÃºmero de observaciones por unidad de tiempo (frequency()) de una serie temporal. AsÃ­, para las temperaturas del aeropuerto de Gran Canaria, donde la unidad de tiempo es el aÃ±o:
deltat(serie1) #fraccion mes dentro del año
frequency(serie1) #estacionalidad (s k explicó el otro día)

# ==============================================================================
# Cargamos los datos necesarios
GET('https://www.ine.es/jaxiT3/files/t/es/xlsx/20193.xlsx', write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- data.frame(read_excel(tf, skip = 6))

# Nos quedamos con las columnas necesarias 
finc <- which(colnames(df) == "Variación.anual")
df <- df[, 1:(finc-1)]
colnames(df) <- df[1, ]
# Nos quedamos con las filas a analizar
finr <- grep("08019", df[, 1])[2]
df2 <- df[finr, ]
fechas <- names(df2)
df <- data.frame(t(df2))
df$fecha <- fechas
rownames(df) <- NULL
df <- df[-1, ] #elimino primera fila pk la primera no tiene nada de importante
colnames(df)[1] <- 'valor'
df$valor <- as.numeric(as.character(df$valor))

# Calculamos los meses y años de cada valor
meses <- as.numeric(unlist(strsplit(df$fecha, split = "M"))[c(F, T)]) #con[c(F,T)] nos permite ir alternando entre false y true y conseguimos los que son True "un truco" 
anyo <- as.numeric(unlist(strsplit(df$fecha, split = "M"))[c(T, F)])
df$meses <- meses
df$anyo <- anyo

# Ordenamos las columnas
df <- df[, c("fecha", "anyo", "meses", "valor")]

# Ordenamos los datos
datos <- df[order(df$anyo, df$meses), ]

# ------------------------------------------------------------------------------
# Convertim les dades a dades a tipus serie temporal
## Es important tenir en compte QUAN s'inicia la sÃ©rie temporal i quina es la 
## freqÃ¼Ã©ncia d'Ãºs.
datosTS <- ts(datos[, 'valor'], frequency = max(datos$meses), start = c(datos[1, 'anyo'], datos[1, 'meses']))

# ==============================================================================
# Grafiquem la sÃ©rie temporal: 
plot.ts(datosTS)

# Descomponemos la serie en las 4 componentes
datosTSDesc <- decompose(datosTS, "additive") #additive/multiplicative es el tipo y la forma canvia por el cálculo
plot(datosTSDesc)

# ==============================================================================
# ModelizaciÃ³n:
## Ruido blanco
naiveModel <- naive(as.numeric(datosTS), h = frequency(datosTS), level = 95)
#o
naiveModel <- naive(datosTS, h = frequency(datosTS), level = 95)
summary(naiveModel)
plot(naiveModel)

acf(naiveModel$fitted[-1], plot = TRUE) 
pacf(naiveModel$fitted[-1], plot = TRUE) 

# ------------------------------------------------------------------------------
# Camino aleatorio
randomModel <- rwf(as.numeric(datosTS), h = frequency(datosTS), level = 95)
summary(randomModel)
plot(randomModel)

acf(randomModel$fitted[-1], plot = TRUE)
pacf(randomModel$fitted[-1], plot = TRUE) 
