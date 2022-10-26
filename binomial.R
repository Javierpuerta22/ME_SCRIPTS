path <-  '/Users/usuari/Desktop/2IA/ME/ME_SCRIPTS/'
data <- read.csv(paste0(path,"database_pre.csv"),sep=";")
train <- read.csv(paste0(path,"train.csv"),sep=";")
test <- read.csv(paste0(path,"test.csv"),sep=";")
#auxiliars
aux.train <- train
aux.test <- test

aux.train$Gender <- ifelse(train$Gender == "M", 0, 1) # Male = 0 / Female = 1
table(aux.train$Gender)

aux.test$Gender <- ifelse(test$Gender == "M", 0, 1) # Male = 0 / Female = 1
table(aux.test$Gender)

#-----------------------------------------------------------------------------------------------------------

respuesta <- "Gender"
barplot(data$EDUC)
hist(aux.train[,respuesta], main = paste0("Histograma de ", respuesta), xlab = respuesta)

aux <- colnames(aux.train)[which(!colnames(aux.train) %in% c(respuesta,"State","Customer.Lifetime.Value","Education","Response","Marital.Status","Policy.Type","Number.of.Policies","Months.Since.Last.Claim", "Coverage", "Location.Code", "Vehicle.Size"))]
explicativas <- paste0(aux, collapse = " + ")
modelo <- paste0(respuesta, " ~ ", explicativas)

#-----------------------------------------------------------------------------------------------------------

# Model logístic
m1 <- glm(paste0("aux.train$",modelo) , family = binomial, data= train)
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

p22 <- predict(modelo, test, type = "response")

p22 <- ifelse(p22 > 0.5, 1,0)
result1 <- table(aux.test$Gender, p22)
result1

accur <- sum(result1[1,2], result1[2,1])/sum(result1)
accur *100

library(vcd)

predicciones <- ifelse(test = modelo$fitted.values > 0.5, yes = 1, no = 0)

mosaic(result1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))


#install.packages("gplots")
library(ROCR)

pred = ROCR::prediction(pred1,aux.test$Gender)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

AUCLog1=performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCLog1,"n")

