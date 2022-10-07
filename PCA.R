# PCA 

#install.packages("FactoMineR")
#install.packages("factoextra")

library(FactoMineR)
library(factoextra)

train <- read.csv("C:/Users/pelot/Desktop/ME_SCRIPTS/train.csv", sep = ";")

clases <- lapply(train, class)      #lista con las clases de cada variable

aux <- subset.data.frame(train, drop = FALSE, select = which(clases == "numeric" | clases == "integer"))

aux2 <- subset.data.frame(train, drop = FALSE, select = which(clases == "character"))


num = list()
cat = list()



for (i in length(clases)){
  if (i == "character"){
    num = c(num, i)
  }
  else{
    cat = c(cat, i)
  }
}


pc <- PCA(aux, scale.unit = TRUE, ncp = 7)

a <- get_pca(pc)

fviz_pca(pc)

a$cor

#---------------------------- porcentaje variables --------------------------------------------





res.pca <- PCA(train, scale.unit = TRUE, quali.sup = c(1,3,4,5,6,7, 9,10,15,17,18), graph=FALSE)


fviz_pca_ind(res.pca, habillage = 15,
             addEllipses =TRUE, ellipse.level = 0.5) +
  scale_color_brewer(palette="Dark2") +
  theme_minimal()
