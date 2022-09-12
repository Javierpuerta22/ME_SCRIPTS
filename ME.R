#------- descargar el csv y fijar una seed para poner los NA ----------

path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"

dd <- read.csv(paste0(path, "database.csv"),sep = ",")

set.seed(1)


#-------- en el caso que salgan las 1034 filas vacías --------

dd <- dd[-(8100:9134),]      #filas vacias

#-------- eliminamos las columnas que hemos decidido quitar ----------
dd <- dd[, c(-1, -7, -16, -19, -21, -20)]

#-------- Ponemos NA aleatorios en la variable escogida ---------

dd$Months.Since.Last.Claim[sample(1:8099, size = 405) ] <- NA

na_en_tabla <- apply(is.na(dd), 2, sum)    #solo dice el número de NA que hay en el database

na_en_tabla


#--------- dataframe con numericas solo ---------------

clases <- lapply(dd, class)      #lista con las clases de cada variable

numericdd <- subset.data.frame(dd, drop = FALSE, select = which(clases == "numeric" | clases == "integer"))

pairs(numericdd)

#---------- funcion descriptiva univariable ---------


info_uni <- function(X, nom){
  print(paste0("le toca la info a la variable ", nom))
  
  if (class(X) == "character"){
    tabla = table(X)
    print(prop.table(tabla))
  }
  else{
    hist(X, main = paste0("Histograma de ", nom))
    summary(X)
  }
  
  
}

#univariable simple

for (i in c(1:22)){
  info_uni(dd[,i], colnames(dd)[i])
}

summary(numericdd)





