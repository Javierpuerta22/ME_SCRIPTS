library(MASS)
library(RcmdrMisc)
library(FactoMineR)

path <-  "/Users/danis.p./Desktop/ME_SCRIPTS/D3/"
dd <- read.csv(paste0(path,"database_pre.csv"),sep=";")

### Perform PCA
?PCA
res.pca<-PCA(dd, ncp = 7 ,quali.sup = c(1,3,4,5,6,7,9,10,15,17,18),graph=FALSE)
res.pca

# Eigenvalues
res.pca$eig

### PCA graphs
?plot.PCA

### Plot for the variables
plot(res.pca,choix="var",axes=c(1,2))
plot(res.pca,choix="var",axes=c(3,4),select = "cos2 0.3")
plot(res.pca,choix="var",axes=c(5,6),select = "cos2 0.3")

### Plot for the individuals
plot(res.pca,choix="ind",invisible="quali",axes=c(1,2),label="none")
plot(res.pca,choix="ind",invisible="quali",axes=c(3,4),label="none")
plot(res.pca,choix="ind",invisible="quali",axes=c(5,6),label="none")

############################################################
# CLUSTERING WITH THE PRINCIPAL COMPONENTS

# Compute distances
Psi<-res.pca$ind$coord
D = dist(Psi)

# Hierarchical clustering
hc <- hclust(D, method = "ward.D2") 
plot(hc)
barplot(hc$height[1:10],main="Aggregated distance at each iteration")

# How many clusters?
abline(h=0.12,col="red",lwd=2)
abline(h=0.08,col="red",lty=2)

# Number of clusters
nc = 5

# Cut tree
clus1<- cutree(hc,nc)
table(clus1)

# Representation of the clusters on the first principal plane
plot(Psi, col=as.numeric(clus1))
abline(h=0,v=0,col="gray")
legend("topright",c("c1","c2","c3","c4","c5"),pch=1,col=c(1:5))

# The quality of partition
cdg <- aggregate(as.data.frame(Psi),list(clus1),mean)[,2:7]
Bss <- sum(rowSums(cdg^2)*as.numeric(table(clus1)))
Tss <- sum(rowSums(Psi^2))
100*Bss/Tss

# Consolidate the partition
clus2<-kmeans(Psi[,2:7],centers=cdg)
clus2$size

Bss <- sum(rowSums(clus2$centers^2)*clus2$size)
Wss <- sum(clus2$withinss)
100*Bss/(Bss+Wss)

# Clusters on the first principal plane after consolidation
plot(Psi, col=as.numeric(clus2$cluster))
abline(h=0,v=0,col="gray")
legend("topright",c("c1","c2","c3","c4","c5"),pch=1,col=c(1:5))

####################################################
#### Hierarchical Clustering on Principle Components with FactoMineR

?HCPC
res.hcpc<-HCPC(res.pca)
names(res.hcpc)

### Description of the partition

### desc.var ###
### A. The description of the clusters by the variables ###
names(res.hcpc$desc.var)
?catdes

### desc.var$test.chi2 ###
### A.1. The categorical variables which characterizes the clusters ###
res.hcpc$desc.var$test.chi2
DictamenXClus<-table(res.hcpc$data.clust$Dictamen,res.hcpc$data.clust$clust)
sweep(DictamenXClus,2,apply(DictamenXClus,2,sum),"/")

### desc.var$category ###
### A.2. The description of each cluster by the categories ##
res.hcpc$desc.var$category
positiuXclust1<-table(res.hcpc$data.clust$Dictamen,res.hcpc$data.clust$clust)[2,1]
positiu<-sum(res.hcpc$data.clust$Dictamen=="positiu",na.rm=TRUE)
clust1<-sum(res.hcpc$data.clust$clust=="1")
N<-nrow(res.hcpc$data.clust)
positiuXclust1/positiu ###Cla/Mod
positiuXclust1/clust1 ###Mod/Cla
positiu/N ###Global

### desc.var$quanti.var ###
### A.3. The quantitative variables which characterizes the clusters ###
res.hcpc$desc.var$quanti.var
summary(lm(res.hcpc$data.clust$Plazo~res.hcpc$data.clust$clust))
summary(lm(res.hcpc$data.clust$Importe.solicitado ~res.hcpc$data.clust$clust))

### desc.var$quanti ###
### A.4. The description of each cluster by the quantitative variables ###
res.hcpc$desc.var$quanti
mean(res.hcpc$data.clust$Edad[res.hcpc$data.clust$clust==1])
mean(res.hcpc$data.clust$Edad)
sd(res.hcpc$data.clust$Edad[res.hcpc$data.clust$clust==1])
sd(res.hcpc$data.clust$Edad)

### desc.axes ###
### B. The description of the clusters by the axes ###
names(res.hcpc$desc.axes)
res.hcpc$desc.axes$quanti.var
res.hcpc$desc.axes$quanti

### desc.ind ###
### C. The description of the clusters by the individuals ###
names(res.hcpc$desc.ind)
res.hcpc$desc.ind$para
res.hcpc$desc.ind$dist

para1<-which(rownames(res.pca$ind$coord)%in%names(res.hcpc$desc.ind$para[[1]]))
para2<-which(rownames(res.pca$ind$coord)%in%names(res.hcpc$desc.ind$para[[2]]))
para3<-which(rownames(res.pca$ind$coord)%in%names(res.hcpc$desc.ind$para[[3]]))
dist1<-which(rownames(res.pca$ind$coord)%in%names(res.hcpc$desc.ind$dist[[1]]))
dist2<-which(rownames(res.pca$ind$coord)%in%names(res.hcpc$desc.ind$dist[[2]]))
dist3<-which(rownames(res.pca$ind$coord)%in%names(res.hcpc$desc.ind$dist[[3]]))

points(res.pca$ind$coord[para1,1],res.pca$ind$coord[para1,2],col="blue",cex=2,pch=16)
points(res.pca$ind$coord[dist1,1],res.pca$ind$coord[dist1,2],col="orange",cex=2,pch=16)
points(res.pca$ind$coord[para2,1],res.pca$ind$coord[para2,2],col="blue",cex=2,pch=16)
points(res.pca$ind$coord[dist2,1],res.pca$ind$coord[dist2,2],col="orange",cex=2,pch=16)
points(res.pca$ind$coord[para3,1],res.pca$ind$coord[para3,2],col="blue",cex=2,pch=16)
points(res.pca$ind$coord[dist3,1],res.pca$ind$coord[dist3,2],col="orange",cex=2,pch=16)

### Characteristics of the more typical and more rare individuals 
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust)%in%names(res.hcpc$desc.ind$para[[1]])),]
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust)%in%names(res.hcpc$desc.ind$dist[[1]])),]
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust)%in%names(res.hcpc$desc.ind$para[[2]])),]
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust)%in%names(res.hcpc$desc.ind$dist[[2]])),]
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust)%in%names(res.hcpc$desc.ind$para[[3]])),]
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust)%in%names(res.hcpc$desc.ind$dist[[3]])),]

### call ###
### Other parameters and objects of HCPC ###
names(res.hcpc$call)

### call$t ###
### Results for the hierarchical tree ###
names(res.hcpc$call$t)
### Results for the PCA ###
res.hcpc$call$t$res
### The suggested level to cut the tree  ###
res.hcpc$call$t$nb.clust
### Within inertias ###
res.hcpc$call$t$within[1:5]
### Ratio between within inertias ###
res.hcpc$call$t$quot[1:5]
### Inertia gain ###
res.hcpc$call$t$inert.gain[1:5]

##########################################################
### Suggested level to cut tree (Original space) 
dcon <- dd[,c(2,4,5,9:16)]
d  <- dist(dcon)
h1 <- hclust(d,method="ward.D")
plot(h1)

library(cluster)
actives<-c(2:16)
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)
distMatrix<-dissimMatrix^2
h2 <- hclust(distMatrix,method="ward.D")
plot(h2)

### Function suggested level
suggested.level<-function(hc,min=3,max=10){
  if(min<2) stop("Min should be equal or higher than 2")
  intra <- rev(cumsum(hc$height))
  quot <- intra[min:(max)]/intra[(min - 1):(max - 1)]
  nb.clust = which.min(quot) + min - 1
  return(nb.clust)
}

### 
suggested.level(h1)
suggested.level(h2)