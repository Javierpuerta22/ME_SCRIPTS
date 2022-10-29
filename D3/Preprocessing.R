#-------------------------Arxiu de preprocess
#------------------ descargar el csv y fijar una seed para poner los NA ----------

path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"

dd <- read.csv(paste0(path, "database.csv"),sep = ",")

library(class)

#-------- en el caso que salgan las 1034 filas vac?as --------

dd <- dd[-(8100:9134),]      #filas vacias

#-------- eliminamos las columnas que hemos decidido quitar ----------
dd <- dd[, c(-1, -7, -16, -19, -21, -20)]



#-------- Ponemos NA aleatorios en la variable escogida ---------

dd$Months.Since.Last.Claim[sample(1:8099, size = 324) ] <- NA

na_en_tabla <- apply(is.na(dd), 2, sum)    #solo dice el numero de NA que hay en el database

na_en_tabla

#-------------------------------------- Mimmi ----------------------------------------------------------

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

#------------------------------------------ imputem les dades --------------------------------------------------

for (varna in varsConNA){
  agr <- aggregate(dd[,varna], by = list(dd$cluster), mean, na.rm=TRUE)
  dd[,paste0(varna, "_imp")] <- agr[match(dd$cluster, agr$Group.1), "x"]
  }
  
dd$Months.Since.Last.Claim <- ifelse(is.na(dd$Months.Since.Last.Claim), dd$Months.Since.Last.Claim_imp, dd$Months.Since.Last.Claim)

sum(is.na(dd))

hist(dd$Months.Since.Last.Claim)

dd$cluster <- NULL
dd$Months.Since.Last.Claim_imp <- NULL

#--------------------------------------------- dividir entre train y test -------------------------------

## 80% of the sample size
smp_size <- floor(0.8 * nrow(dd))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dd)), size = smp_size)

train <- dd[train_ind, ]
test <- dd[-train_ind, ]

# ------------------------------------------ modifiquem el nom de les categories ---------------------------

var_fact = c("State", "Coverage", "Education", "EmploymentStatus", "Location.Code", "Marital.Status", "Policy.Type", "Vehicle.Class")

dd[, var_fact] <- lapply(dd[, var_fact], as.factor)

colnames(dd) <- c("ST", "CLV", "Resp.", "COV", "EDUC", "EMPS", "GEN", "INC", "LOCC", "MARS", "MPA", "MSLC", "MSPI", "NOP", "PT", "TCA", "VC", "VS")

#Cambiar etiquetas
levels(dd$ST) <- c("AR", "CAL", "NEV", "OR", "WASH")
levels(dd$Cov.) <- c("BAS", "EXT", "PREM")
levels(dd$EDUC) <- c("BACH", "COLL", "DOCT", "HSOB", "MAST")
levels(dd$EMPS) <- c("DIS", "EMP", "ML", "RET", "UNEMP")
levels(dd$LOCC) <- c("RUR", "SUBU", "URB")
levels(dd$MARS) <- c("DIV", "MAR", "SING")
levels(dd$PT) <- c("CORP", "PERS", "SPEC")
levels(dd$VC) <- c("4D", "LUX", "LUXS", "SPO", "SUV", "2D")
levels(dd$VS) <- c("L", "M", "S")

# ------------------ escrivim noves databases ------------------------------------------------------------------

write.table(test, file = "test.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
write.table(train, file = "train.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
write.table(dd, file = "database_pre.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)



