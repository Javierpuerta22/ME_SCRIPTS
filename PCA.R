# PCA 

install.packages("FactoMineR")
install.packages("factoextra")

library(FactoMineR)
library(factoextra)

train <- read.csv("C:/Users/pelot/Desktop/ME_SCRIPTS/train.csv", sep = ";")

clases <- lapply(train, class)      #lista con las clases de cada variable

aux <- subset.data.frame(train, drop = FALSE, select = which(clases == "numeric" | clases == "integer"))

pc <- PCA(aux, scale.unit = TRUE)
