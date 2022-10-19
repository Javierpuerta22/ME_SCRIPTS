library(FactoMineR)
library(factoextra)
library(ggplot2)

train <- read.csv("C:/Users/pelot/Desktop/ME_SCRIPTS/database_pre.csv", sep = ";")

var_fact = c("ST", "COV", "EDUC", "EMPS", "LOCC", "MARS", "PT", "VC", "VS")

train[, var_fact] <- lapply(train[, var_fact], as.factor)


clases <- lapply(train, class)      #lista con las clases de cada variable

aux2 <- subset.data.frame(train, drop = FALSE, select = which(clases == "factor"))

MC <- MCA(aux2)

fviz_mca(MC)
