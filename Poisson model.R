path <- "C:/Users/adria/IA/3r Quadri/ME/ME_SCRIPTS/"
data <- read.csv(paste0(path,"train.csv"),sep=";")

plot(data, pch = as.numeric(data$Monthly.Premium.Auto), col = as.numeric(data$Monthly.Premium.Auto))

modelo <- glm(Monthly.Premium.Auto ~ Months.Since.Last.Claim + Total.Claim.Amount , family = poisson, data = data)
modelo

modelo.completo <- glm(Monthly.Premium.Auto ~ . , family = poisson, data = data)
summary(modelo.completo)

library(MASS)
library(RcmdrMisc)
modelo <- stepwise(modelo.completo, direction='backward/forward', criterion='BIC')
plot(modelo)

p.est <- predict(modelo, type = "response")

cat.est <- as.numeric(p.est > 0.5)
tabla <- table(data$Montlhy.Premium.Auto, cat.est)
tabla
