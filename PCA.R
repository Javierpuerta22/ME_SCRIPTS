# PCA 

install.packages("FactoMineR")
install.packages("factoextra")

library(FactoMineR)
library(factoextra)
library(ggplot2)

train <- read.csv("D:/UPC/ME/ME_SCRIPTS/train.csv", sep = ";")

clases <- lapply(train, class)      #lista con las clases de cada variable

aux <- subset.data.frame(train, drop = FALSE, select = which(clases == "numeric" | clases == "integer"))

pc <- PCA(aux, scale.unit = TRUE)

## PLOTS =============================================================================================================

## GRAPHS OF INDIVIDUALS ---------------------------------------------------------------------------------------------

# Color according to their contribution, without the individuals' tag (colors)
fviz_pca_ind(pc,geom="point",pointsize=1.5,col.ind="contrib") + scale_color_gradient(low="blue", high="red")

# Color according to their contribution, with the individuals' tag (colors)
fviz_pca_ind(pc, col.ind="contrib") + scale_color_gradient(low="blue", high="red") + theme_minimal()

# Select the top 20 contributing individuals
fviz_pca_ind(pc, select.ind = list(contrib = 20))

# Color individuals by groups (FALTA MODIFICAR "habillage" PARA NUESTRA BASE DE DATOS)
fviz_pca_ind(pc, label="none", habillage=iris$Species)


## GRAPHS OF VARIABLES -----------------------------------------------------------------------------------------------

# Using points and naming the variables (no arrows)
fviz_pca_var(pc, geom = c("point", "text"))

# According to their contributions, with arrows and var tags (colors)
fviz_pca_var(pc, col.var="contrib")+ scale_color_gradient2(low="cornflowerblue",
                        mid="red", midpoint=50) + theme_minimal()


## BIPLOTS OF INDIVIDUALS OF VARIABLES ===============================================================================

# Keep only the labels for variables
fviz_pca_biplot(pc, label ="var")

# Change the color by groups, add ellipses  (FALTA MODIFICAR "habillage" PARA NUESTRA BASE DE DATOS)
fviz_pca_biplot(pc, label="var", habillage=iris$Species,
                addEllipses=TRUE, ellipse.level=0.95)

# Color according to the individuals' contribution 
fviz_pca_biplot(pc, geom = "point", pointsize = 1.5, col.ind="contrib") + scale_color_gradient(low="blue", high="red")
