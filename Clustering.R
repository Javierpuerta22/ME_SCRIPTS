#clustering

path <- '/Users/danis.p./Desktop/ME_SCRIPTS/'
test <- read.csv(paste0(path, "test.csv"), sep = ";")
data <- read.csv(paste0(path, "train.csv"), sep = ";")

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

plot <- fviz_cluster(k1, data=dcon, geom = "point")
plot
