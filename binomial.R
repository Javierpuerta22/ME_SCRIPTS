
path <-  '/Users/liza/Desktop/UNIVER/2.1/ME/ME_SCRIPTS/'
fichero <- 'database_pre.csv'
dd <- read.csv(paste0(path, fichero), sep = ";")
head(dd)

# Modifiquem el nom d'alguns nivells de factor
dd$Gender <- ifelse(dd$Gender == "M", 0, 1) # Male = 0 / Female = 1
table(dd$Gender)

# Dividim les dades en dos grups: entrenament(70 %) i prova(30 %)
n <- dim(dd)[1]

set.seed(1234)
train <- sample(1:n, 0.8*n)

dd.train <- dd[train,]
dd.test <- dd[-train,]

ytrain <- dd$Gender[train]
ytest <- dd$Gender[-train]


# Model logístic
m1 <- glm(Gender ~ . , family = binomial, data = dd.train)
summary(m1)

# Variables significativas
sig.var <- summary(m1)$coeff[-1,4] < 0.01
names(sig.var)[sig.var == TRUE]

# Predim amb el model logístic el conjunt test
pred1 <- predict.glm(m1,newdata = dd.test, type="response")
result1 <- table(ytest, floor(pred1+0.5))
result1

error1 <- sum(result1[1,2], result1[2,1])/sum(result1)
error1



install.packages("gplots")
library(ROCR)

pred = ROCR::prediction(pred1,ytest)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

AUCLog1=performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCLog1,"n")
