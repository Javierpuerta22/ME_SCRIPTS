#dd <- read.csv("C:/Users/adria/IA/3r Quadri/ME/ME_SCRIPTS/ME_SCRIPTS/database.csv")


varsConNA <- names(which(colSums(is.na(train))>0))
varsSinNA <- colnames(train)[which(!colnames(train) %in% varsConNA)]

library(cluster)


agggr <- sapply(train, class)
varsCat <- names(agggr)[which(agggr == "character")]

for (varCat in varsCat) {
  train[, varCat] <- as.factor(train[, varCat])
}

dissimMatrix <- daisy(train[, varsSinNA], metric = "gower", stand=TRUE)

distMatrix <- dissimMatrix^2

h1 <- hclust(distMatrix, method = "ward.D2")
plot(h1)

c2 <- cutree(h1, 5)
train[,"cluster"] <- c2


for (varNA in varsConNA) {
  agr <- aggregate(train[,varNA], by = list(train$cluster), mean, na.rm=TRUE)
  train[,paste0(varNA, "_imp")] <- agr[match(train$cluster, agr$Group.1), "x"]
}


             
             
table(c2)

table(c1,c2)