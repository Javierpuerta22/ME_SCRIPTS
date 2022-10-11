# PCA 

#install.packages("FactoMineR")
#install.packages("factoextra")

library(FactoMineR)
library(factoextra)
library(ggplot2)

train2 <- read.csv("C:/Users/pelot/Desktop/ME_SCRIPTS/train_fact.csv", sep = ";")

var_fact = c("ST", "COV", "EDUC", "EMPS", "LOCC", "MARS", "PT", "VC", "VS")

train2[, var_fact] <- lapply(train2[, var_fact], as.factor)


train <- read.csv("C:/Users/pelot/Desktop/ME_SCRIPTS/train.csv", sep = ";")

clases <- lapply(train2, class)      #lista con las clases de cada variable

aux <- subset.data.frame(train, drop = FALSE, select = which(clases == "numeric" | clases == "integer"))


aux2 <- subset.data.frame(train2, drop = FALSE, select = which(clases == "factor"))


MC <- MCA(aux2)

fviz_mca(MC)

pc <- PCA(aux, scale.unit = TRUE, ncp = 7)

a <- get_pca(pc)

fviz_pca(pc)

a$cor

#---------------------------- porcentaje variables --------------------------------------------





pc <- PCA(train, scale.unit = TRUE, quali.sup = c(1,2,3,4,5,15,16,17,18), graph=FALSE)


fviz_pca_ind(res.pca, habillage = 15,
             addEllipses =TRUE, ellipse.level = 0.5) +
  scale_color_brewer(palette="Dark2") +
  theme_minimal()

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
fviz_pca_ind(pc, label="none", habillage=2)


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
fviz_pca_biplot(pc, label="var", habillage=7,
                addEllipses=TRUE, ellipse.level=0.95)

# Color according to the individuals' contribution 
fviz_pca_biplot(pc, geom = "point", pointsize = 1.5, col.ind="contrib") + scale_color_gradient(low="blue", high="red")

