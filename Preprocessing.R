#Arxiu de preprocess
#------- descargar el csv y fijar una seed para poner los NA ----------

path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"

dd <- read.csv(paste0(path, "database.csv"),sep = ",")

library(class)

#-------- en el caso que salgan las 1034 filas vac?as --------

dd <- dd[-(8100:9134),]      #filas vacias

#-------- eliminamos las columnas que hemos decidido quitar ----------
dd <- dd[, c(-1, -7, -16, -19, -21, -20)]

#-------- dividir entre train y test -------------------------------

## 80% of the sample size
smp_size <- floor(0.8 * nrow(dd))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dd)), size = smp_size)

train <- dd[train_ind, ]
test <- dd[-train_ind, ]

#-------- Ponemos NA aleatorios en la variable escogida ---------

train$Months.Since.Last.Claim[sample(1:6479, size = 324) ] <- NA

na_en_tabla <- apply(is.na(train), 2, sum)    #solo dice el numero de NA que hay en el database

na_en_tabla


#-------- tratamos los NA con knn -----------------------

attach(train)

#Primer agafem totes les numèriques menys la variable que volem imputar

clases <- lapply(train, class)      #lista con las clases de cada variable

aux <- subset.data.frame(train, drop = FALSE, select = which(clases == "numeric" | clases == "integer"))

aux <- aux[, - 4]

#Dividim els valors entre NA i no NA

aux1 <- aux[!is.na(Months.Since.Last.Claim),]

aux2 <- aux[is.na(Months.Since.Last.Claim),]

#Fem la funció knn per estudiar els valors que hem de col·locar en els NA i els imputem

knn.ing <- knn(aux1, aux2, Months.Since.Last.Claim[!is.na(Months.Since.Last.Claim)])

Months.Since.Last.Claim[is.na(Months.Since.Last.Claim)] <- as.numeric(levels(knn.ing))

#Apliquem els valors ja imputats a la database i la creem

train$Months.Since.Last.Claim <- Months.Since.Last.Claim

write.table(train, file = "train.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
write.table(test, file = "test.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)



