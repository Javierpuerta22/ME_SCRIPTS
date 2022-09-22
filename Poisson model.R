path <- "C:/Users/adria/IA/3r Quadri/ME/ME_SCRIPTS/"
dd <- read.csv(paste0(path,"database_pre.csv"),sep=";")
df <- as.data.frame(dd)


## 80% of the sample size
smp_size <- floor(0.8 * nrow(dd))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dd)), size = smp_size)

train <- dd[train_ind, ]
test <- dd[-train_ind, ]
df <- as.data.frame(train)
data <- df[-100, c(6:12, 16)]
#Variable amb distribució de poisson = Monthly.Premium.Auto

plot(data, pch = as.numeric(data$Monthly.Premium.Auto), col = as.numeric(data$Monthly.Premium.Auto))

modelo <- glm(Monthly.Premium.Auto ~ Months.Since.Last.Claim + Total.Claim.Amount , family = poisson, data = data)
modelo

modelo.completo <- glm(Monthly.Premium.Auto ~ . , family = poisson, data = data)
summary(modelo.completo)

library(MASS)
library(RcmdrMisc)
modelo <- stepwise(modelo.completo, direction='backward/forward', criterion='BIC')

p.est <- predict(modelo, type = "response")

cat.est <- as.numeric(p.est > 0.5)
tabla <- table(datos$Montlhy.Premium.Auto, cat.est)
tabla
