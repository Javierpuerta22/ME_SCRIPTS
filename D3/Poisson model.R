#install.packages("Metrics")
library(Metrics)
library(MASS)
library(RcmdrMisc)

path <- "C:/Users/adria/IA/3r Quadri/ME/ME_SCRIPTS/D3/"
train <- read.csv(paste0(path,"train.csv"),sep=";")
test <- read.csv(paste0(path,"test.csv"),sep=";")
source(paste0(path, "functions.R"))

# --------------------------------- Variables explicatives --------------------------------

respuesta <- "Number.of.Policies"
hist(train[,respuesta], main = paste0("Histograma de ", respuesta), xlab = respuesta)
no_queremos <- c("State","Response", "EmploymentStatus","Income","Education","Months.Since.Policy.Inception","Months.Since.Last.Claim")

respuesta <- "Customer.Lifetime.Value"
hist(train[,respuesta], main = paste0("Histograma de ", respuesta), xlab = respuesta)
no_queremos <- c()

respuesta <- "Months.Since.Policy.Inception"
hist(train[,respuesta], main = paste0("Histograma de ", respuesta), xlab = respuesta)
no_queremos <- c("State","Education","Vehicle.Class")



modelo <- create_formula(train, respuesta, no_queremos)

# ---------------------------------- Model binomial ----------------------------------------

modelo.completo <- glm(modelo, family = poisson(link = "log"), data = train)
summary(modelo.completo)



modelo <- stepwise(modelo.completo, direction='backward/forward', criterion='AIC')
summary(modelo)
plot(modelo)

p.est <- predict(modelo, newdata = test, type = "response")

#---------------------------------- Performance del model ---------------------------------------

<<<<<<< HEAD
performance <- cor(p.est,test$Number.of.Policies)

#---------------------------------- Root mean square error ---------------------------------------

aux <- rmse(test$Number.of.Policies,p.est)
acc <- aux/mean(test$Number.of.Policies)
acc

aux2 <- rmse(test$Customer.Lifetime.Value,p.est)
acc2 <- aux2/mean(test$Customer.Lifetime.Value)
acc2
=======
a <- rmse(test$Number.of.Policies, p.est)

desv_porc <- a/mean(test$Number.of.Policies)
desv_porc
>>>>>>> 19b33c917b1517a6fa3f32745a90a4d74bf1e662
