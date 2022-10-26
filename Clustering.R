#clustering
<<<<<<< HEAD

path <- '/Users/danis.p./Desktop/ME_SCRIPTS/'
test <- read.csv(paste0(path, "test.csv"), sep = ";")
data <- read.csv(paste0(path, "train.csv"), sep = ";")
=======
path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"
data <- read.csv(paste0(path,"database_pre.csv"), sep = ";")
>>>>>>> 659a54ce56dbc1a2be7133a2c54819c1417f9525


sum(is.na(data))

attach(data)

dcon <- data.frame(Customer.Lifetime.Value,Income,Monthly.Premium.Auto,Months.Since.Last.Claim,Months.Since.Policy.Inception, Number.of.Policies)
dim(dcon)

sapply(data,class)

k <- 4
k1 <- kmeans(dcon,k)
names(dcon)
print(k1)

attributes(k1)

k1$size

k1$withins

k1$centers


Bss <- sum(rowSums(k1$centers^2)*k1$size)

Wss <- sum(k1$withinss)

Tss <- k1$totss


Ib1 <- 100*Bss/(Bss+Wss)
Ib1

data[,ncol(data)+1] <- k1$cluster
names(data)[ncol(data)] <- "Kmeans"

<<<<<<< HEAD
plot <- fviz_cluster(k1, data=dcon, geom = "point")
plot
=======
write.table(data, file = "database_cluster.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)



for (i in colnames(dcon)){
  plot(data[,i], data[, next(i)])
}
colores = c("Red", "Blue", "Green", "Yellow")
plot(data$Income, data$Customer.Lifetime.Value, col= colores[data$Kmeans])
<<<<<<< HEAD
>>>>>>> 659a54ce56dbc1a2be7133a2c54819c1417f9525
=======

#clust jeràrquic

d <- dist(dcon)
h1 <- hclust(d,method="ward.D")
plot(h1)

nc = #Nombre de talls del dendograma
  
c1 <- cutree(h1,nc)
c1[1:20]

nc = #un altre nombre de talls
  
c2 <- cutree(h1,nc)

table(c1,c2)

cdg <- aggregate(as.data.frame(dcon),list(c1),mean)

library(cluster)

actives <- c(#Variables a utilitzar)
dissimMatrix <- daisy(dd[,actives], metric = "gower",stand=TRUE)
  
dissimMatrix <- dissimMatrix^2
h1 <- hclust(dissimMatrix, method="ward.D")

plot(h1)

c3 <- cutree(h1,#Nombre de talls)
table(c3)

#Profiling
>>>>>>> 2ac55367bbcaade7336ffcba2bd6a455c239be0b
