path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"
data <- read.csv(paste0(path,"train.csv"),sep=";")

#plot(data, pch = as.numeric(data$Monthly.Premium.Auto))

modelo <- glm(Monthly.Premium.Auto ~ Months.Since.Last.Claim + Total.Claim.Amount , family = poisson, data = data)
modelo

modelo.completo <- glm(Customer.Lifetime.Value ~ . , family = poisson(link = "log"), data = data)
summary(modelo.completo)

library(MASS)
library(RcmdrMisc)
modelo <- stepwise(modelo.completo, direction='backward/forward', criterion='BIC')
plot(modelo)

confint(modelo)

p.est <- predict(modelo, type = "response")


tabla <- table(data$Monthly.Premium.Auto, p.est)
tabla

accuracy <- sum(tabla[1,1], tabla[2,2])/sum(tabla)
accuracy *100

