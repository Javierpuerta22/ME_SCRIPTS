train <- read.csv("C:/Users/pelot/Desktop/ME_SCRIPTS/train.csv", sep = ";")


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
  agr <- aggregate(train[,is.na(varNA)], by = list(train$cluster), mean, na.rm=TRUE)
  train[,paste0(varNA, "_imp")] <- agr[match(train$cluster, agr$Group.1), "x"]
}


hist(dd$Months.Since.Last.Claim) 

hist(train$Months.Since.Last.Claim)

train$cluster <- NULL

write.table(train, file = "train.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
