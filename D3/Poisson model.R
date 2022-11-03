library(MASS)
library(RcmdrMisc)

path <- "C:/Users/adria/IA/3r Quadri/ME/ME_SCRIPTS/ME_SCRIPTS/"
train <- read.csv(paste0(path,"train.csv"),sep=";")
test <- read.csv(paste0(path,"test.csv"),sep=";")
source(paste0(path, "functions.R"))

# --------------------------------- Variables explicatives --------------------------------

respuesta <- "Number.of.Policies"
hist(train[,respuesta], main = paste0("Histograma de ", respuesta), xlab = respuesta)
no_queremos <- c("State","Response", "EmploymentStatus","Income","Education","Months.Since.Policy.Inception","Months.Since.Last.Claim")

modelo <- create_formula(train, respuesta, no_queremos)

# ---------------------------------- Model binomial ----------------------------------------

modelo.completo <- glm(modelo, family = poisson(link = "log"), data = data)
summary(modelo.completo)


modelo <- stepwise(modelo.completo, direction='backward/forward', criterion='AIC')
summary(modelo)
plot(modelo)

p.est <- predict(modelo, newdata = test, type = "response")

#---------------------------------- Performance del model ---------------------------------------

a <- rmse(test$Number.of.Policies, p.est)

desv_porc <- a/mean(test$Number.of.Policies)
desv_porc
