#clustering
path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"
data <- read.csv(paste0(path,"database_pre.csv"), sep = ";")


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

write.table(data, file = "database_cluster.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)



for (i in colnames(dcon)){
  plot(data[,i], data[, next(i)])
}

colores = c("Red", "Blue", "Green", "Yellow")

plot(data$Income, data$Customer.Lifetime.Value, col= colores[data$Kmeans])
