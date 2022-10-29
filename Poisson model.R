library(MASS)
library(RcmdrMisc)

path <- "C:/Users/adria/IA/3r Quadri/ME/ME_SCRIPTS/ME_SCRIPTS/"
train <- read.csv(paste0(path,"train.csv"),sep=";")
test <- read.csv(paste0(path,"test.csv"),sep=";")

# --------------------------------- Variables explicatives --------------------------------

respuesta <- "Months.Since.Policy.Inception"
hist(train[,respuesta], main = paste0("Histograma de ", respuesta), xlab = respuesta)

aux <- colnames(train)[which(!colnames(train) %in% c(respuesta,"State", "Education", "Vehicle.Class","EmploymentStatus", "Vehicle.Size"))]
explicativas <- paste0(aux, collapse = " + ")
modelo <- paste0(respuesta, " ~ ", explicativas)

# ---------------------------------- Model binomial ----------------------------------------

modelo.completo <- glm(modelo, family = poisson(link = "log"), data = data)
summary(modelo.completo)


modelo <- stepwise(modelo.completo, direction='backward/forward', criterion='AIC')
summary(modelo)
plot(modelo)

#---------------------------------- Performance del model ---------------------------------------

p.est <- predict(modelo, newdata = test, type = "response")
cor(p.est,test$Months.Since.Last.Claim)
#|Cor| of 0.64
