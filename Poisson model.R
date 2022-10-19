path <- "C:/Users/adria/IA/3r Quadri/ME/ME_SCRIPTS/ME_SCRIPTS/"
data <- read.csv(paste0(path,"train.csv"),sep=";")
test <- read.csv(paste0(path,"test.csv"),sep=";")


hist(data$Months.Since.Last.Claim)

modelo.completo <- glm(Months.Since.Last.Claim ~ . , family = poisson(link = "log"), data = data)
summary(modelo.completo)

library(MASS)
library(RcmdrMisc)
#install.packages("arm")

modelo <- stepwise(modelo.completo, direction='backward/forward', criterion='AIC')
summary(modelo)
plot(modelo)

library(arm)
coef1 = coef(modelo)
coef1

coef2 = se.coef(modelo)
coef2

fitted(modelo)

p.est <- predict(modelo, newdata = test, type = "response")

tabla <- table(test$Months.Since.Last.Claim, p.est)
summary(tabla)
