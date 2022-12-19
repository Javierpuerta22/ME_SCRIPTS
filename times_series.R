path <- "C:/Users/adria/IA/3r Quadri/ME/ME_SCRIPTS/"
dt <- read.csv(paste0(path,"Dataframe temporal.csv"),sep=",")
meses <- as.numeric(unlist(strsplit(dt$DATE,split='-'))[c(F,T,F)])
anyos <- as.numeric(unlist(strsplit(dt$DATE,split='-'))[c(T,F,F)])

dt$meses <- meses
dt$anyos <- anyos
dt <-dt[,c("DATE","anyos","meses","TOTALSA")]
datos <- dt[order(dt$anyo,dt$meses),]
datosTS <- ts(datos[,'TOTALSA'],frequency=max(datos$meses),start=c(datos[1,'anyos'],datos[1,'meses']))
plot.ts(datosTS)

datosTSDesc <- decompose(datosTS,"additive")
plot(datosTSDesc)

library(forecast)


DanielsContrast(datosTS)
KWContrast(datosTS)

# Contrasts #

# Contraste de Daniels
DanielsContrast <- function(serieTS, tipo = "meses", conclusion = TRUE, conf = 0.95) {
  tabla <- data.frame(Yt = serieTS)
  tabla$fecha <- as.character(zoo::as.yearmon(time(serieTS)))
  tabla$anyo <- unlist(strsplit(tabla$fecha, ". ", fixed = FALSE))[c(FALSE, TRUE)]
  tabla$s <- unlist(strsplit(tabla$fecha, ". ", fixed = FALSE))[c(TRUE, FALSE)]
  
  if (tipo == "meses") {
    meses <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
    tabla$s <- match(tabla$s, meses)
  }
  
  # Se deberia de implementar de la misma forma para otros tipos de s
  # ordenamos por s y luego por aÃ±o
  tabla <- tabla[order(tabla$anyo, tabla$s), ]
  
  # Rango a travÃ©s del tiempo
  tabla$t <- 1:nrow(tabla)
  
  # Rango a travÃ©s del valor Yt
  tabla <- tabla[order(tabla$Yt), ]
  tabla$rangoYt <- 1:nrow(tabla)
  
  tabla$dt2 <- (tabla$rangoYt - tabla$t)^2
  
  # Calculamos el estadÃ­stico
  Tm <- max(tabla$t)
  tau <- 1 - ((6*sum(tabla$dt2)) / (Tm*((Tm^2) - 1)))
  
  # NormalizaciÃ³n
  z <- abs(sqrt(Tm - 1)*tau)
  
  Ztab <- qnorm((1 - conf)/2, lower.tail = FALSE)
  
  if (conclusion) {
    if (z > Ztab) {
      concls <- "La sÃ©rie tiene tendÃ©ncia"  
    } else {
      concls <- "La sÃ©rie NO tiene tendÃ©ncia"  
    }
    
    cat("===============================================\n")
    cat("Valor de tau: ", tau, "\n")
    cat("Valor de |z|: ", z,   "\n")
    cat("Valor de Z tablas: ", Ztab, "\n")
    cat("ConclusiÃ³n: ", concls, "\n")
    cat("===============================================")
  } else {
    cat("===============================================")
    cat("Valor de tau:", tau, "\n")
    cat("Valor de |z|:", z,   "\n")
    cat("Valor de Z tablas:", Ztab, "\n")
    cat("===============================================")
  }
}

# ==============================================================================

KWContrast <- function(serieTS, tipo = "meses", conclusion = TRUE, conf = 0.95) {
  tabla <- data.frame(Yt = serieTS)
  tabla$fecha <- as.character(zoo::as.yearmon(time(serieTS)))
  tabla$anyo <- unlist(strsplit(tabla$fecha, ". ", fixed = FALSE))[c(FALSE, TRUE)]
  tabla$s <- unlist(strsplit(tabla$fecha, ". ", fixed = FALSE))[c(TRUE, FALSE)]
  
  if (tipo == "meses") {
    meses <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
    tabla$s <- match(tabla$s, meses)
  }
  
  # Se deberia de implementar de la misma forma para otros tipos de s
  # ordenamos por s y luego por aÃ±o
  tabla <- tabla[order(tabla$anyo, tabla$s), ]
  
  # Rango a travÃ©s del tiempo
  tabla$t <- 1:nrow(tabla)
  
  # Rango a travÃ©s del valor Yt
  tabla <- tabla[order(tabla$Yt), ]
  tabla$rangoYt <- 1:nrow(tabla)
  
  # Calculamos una tabla externa
  tablaExt <- data.frame(S = unique(sort(tabla$s)))
  
  valorTi <- c(); valorRi <- c()
  for (i in tablaExt$S) {
    valorTi <- c(valorTi, sum(tabla$s == i))
    valorRi <- c(valorRi, sum(tabla[which(tabla$s == i), 'rangoYt']))
  }
  
  tablaExt$Ti <- valorTi; tablaExt$Ri <- valorRi
  tablaExt$Ri2_Ti <- (tablaExt$Ri^2)/tablaExt$Ti
  
  Tm <- max(tabla$t)
  
  H <- (12/(Tm*(Tm + 1)))*sum(tablaExt$Ri2_Ti)-3*(Tm + 1)
  
  chis <- qchisq(conf, nrow(tablaExt)-1)
  
  if (conclusion) {
    if (H > chis) {
      concls <- "La sÃ©rie tiene estacionalidad"  
    } else {
      concls <- "La sÃ©rie NO tiene estacionalidad"  
    }
    
    cat("===============================================\n")
    cat("Valor de H: ", H, "\n")
    cat("Valor de Chisq tablas: ", chis, "\n")
    cat("ConclusiÃ³n: ", concls, "\n")
    cat("===============================================")
  } else {
    cat("===============================================")
    cat("Valor de H: ", H, "\n")
    cat("Valor de Chisq tablas: ", chis, "\n")
    cat("===============================================")
  }  
}
