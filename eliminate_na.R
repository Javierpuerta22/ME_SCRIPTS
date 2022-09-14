path <- "C:/Users/adria/IA/3r Quadri/ME/ME_SCRIPTS/"

library(class)

dd <- read.csv(paste0(path, "database_na.csv"),sep = ";")

apply(is.na(dd), 2, sum) #Veiem que existeien NA

attach(dd)

#------ treiem els NA A apartir de MIMMI ----------------

#Primer agafem totes les numèriques menys la variable que volem imputar

clases <- lapply(dd, class)      #lista con las clases de cada variable

aux <- subset.data.frame(dd, drop = FALSE, select = which(clases == "numeric" | clases == "integer"))

aux <- aux[, - 4]

#Dividim els valors entre NA i no NA

aux1 <- aux[!is.na(Months.Since.Last.Claim),]

aux2 <- aux[is.na(Months.Since.Last.Claim),]

#Fem la funció knn per estudiar els valors que hem de col·locar en els NA i els imputem

knn.ing <- knn(aux1, aux2, Months.Since.Last.Claim[!is.na(Months.Since.Last.Claim)])


Months.Since.Last.Claim[is.na(Months.Since.Last.Claim)] <- as.numeric(levels(knn.ing))

#Veiem la diferencia entre les dos variables amb NA i sense en un histograma

hist(Months.Since.Last.Claim)

hist(dd$Months.Since.Last.Claim)

#Apliquem els valors ja imputats a la database i la creem

dd$Months.Since.Last.Claim <- Months.Since.Last.Claim

write.table(dd, file = "database_pre.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)



