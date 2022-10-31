#---------------------------------------------- ARCHIVO CON EL MODELO ANCOVA ---------------------------
library(MASS)
library(car)

path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/D3/"
train <- read.csv(paste0(path, "train.csv"),sep = ";")
test <- read.csv(paste0(path, "test.csv"), sep = ";")
source(paste0(path, "functions.R"))
attach(train)

#------------------------------- mirar los supuestos de ANCOVA --------------------

train$Policy.Type <- as.factor(train$Policy.Type)

# Igualdad de variancias, debe dar no significativo

leveneTest(train$Total.Claim.Amount, train$Policy.Type, center = median)

# Independencia, debe dar no significativo

mod1 <- aov(Income ~ Policy.Type, data = train)
anova(mod1)

# homogeniedad de las pendientes de regresión, debe dar no significativo

mod2 <- aov(Total.Claim.Amount ~ Policy.Type * Income , data = train)
anova(mod2)

# ----------------------------- Modelo ---------------------------------------------

ancova <- lm("Total.Claim.Amount ~ Policy.Type + Income", data = train)
summary(ancova)
plot(ancova)

#------------------------------- Performance ---------------------------------------

performance <- 0.1212 #es el valor del R^2

