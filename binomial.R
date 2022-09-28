path <-  '/Users/usuari/Desktop/2IA/ME/ME_SCRIPTS/'
train <- read.csv(paste0(path,"train.csv"),sep=";")
test <- read.csv(paste0(path,"test.csv"),sep=";")

aux.train <- train
aux.test <- test

aux.train$Gender <- ifelse(train$Gender == "M", 0, 1) # Male = 0 / Female = 1
table(aux.train$Gender)

aux.test$Gender <- ifelse(test$Gender == "M", 0, 1) # Male = 0 / Female = 1
table(aux.test$Gender)


# Model logístic
m1 <- glm(aux.train$Gender ~ . , family = binomial, data= train)
summary(m1)
plot(m1)

library(MASS)
library(RcmdrMisc)
modelo <- stepwise(m1, direction='backward/forward', criterion='AIC')
plot(modelo)

summary(modelo)

# Variables significativas
sig.var <- summary(m1)$coeff[-1,4] < 0.01
names(sig.var)[sig.var == TRUE]

# Predim amb el model logístic el conjunt test
pred1 <- predict.glm(m1,newdata = test, type="response")
result1 <- table(aux.test$Gender, floor(pred1+0.5))
result1

error1 <- sum(result1[1,2], result1[2,1])/sum(result1)
error1



install.packages("gplots")
library(ROCR)

pred = ROCR::prediction(pred1,aux.test$Gender)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

AUCLog1=performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCLog1,"n")

