#------- descargar el csv y fijar una seed para poner los NA ----------

path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"

dd <- read.csv(paste0(path, "database.csv"),sep = ",")

set.seed(1)


#-------- en el caso que salgan las 1034 filas vac?as --------

dd <- dd[-(8100:9134),]      #filas vacias

#-------- eliminamos las columnas que hemos decidido quitar ----------
dd <- dd[, c(-1, -7, -16, -19, -21, -20)]

#-------- Ponemos NA aleatorios en la variable escogida ---------

dd$Months.Since.Last.Claim[sample(1:8099, size = 405) ] <- NA


write.table(dd, file = "database_na.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)


na_en_tabla <- apply(is.na(dd), 2, sum)    #solo dice el numero de NA que hay en el database


na_en_tabla


#--------- dataframe con numericas solo i la correlación entre ellas ---------------

clases <- lapply(dd, class)      #lista con las clases de cada variable

numericdd <- subset.data.frame(dd, drop = FALSE, select = which(clases == "numeric" | clases == "integer"))

pairs(numericdd)

corrplot::corrplot(cor(numericdd), method = "number")


