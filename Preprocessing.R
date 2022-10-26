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

#---------- Mimmi ----------------------------------------------------------

hist(dd$Months.Since.Last.Claim) 

varsConNA <- names(which(colSums(is.na(dd))>0))
varsSinNA <- colnames(dd)[which(!colnames(dd) %in% varsConNA)]

library(cluster)


agggr <- sapply(dd, class)
varsCat <- names(agggr)[which(agggr == "character")]

for (varCat in varsCat) {
  dd[, varCat] <- as.factor(dd[, varCat])
}

dissimMatrix <- daisy(dd[, varsSinNA], metric = "gower", stand=TRUE)

distMatrix <- dissimMatrix^2

h1 <- hclust(distMatrix, method = "ward.D2")
plot(h1)

c2 <- cutree(h1, 5)
dd[,"cluster"] <- c2


for (varNA in varsConNA) {
  agr <- aggregate(dd[,is.na(varNA)], by = list(dd$cluster), mean, na.rm=TRUE)
  dd[,paste0(varNA, "_imp")] <- agr[match(dd$cluster, agr$Group.1), "x"]
}


hist(dd$Months.Since.Last.Claim)

dd$cluster <- NULL

Months.Since.Last.Claim[is.na(Months.Since.Last.Claim)] <- as.numeric(levels(knn.ing))

#Apliquem els valors ja imputats a la database i la creem

train$Months.Since.Last.Claim <- Months.Since.Last.Claim

write.table(test, file = "test.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
write.table(train, file = "train.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
write.table(dd, file = "database_pre.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)



