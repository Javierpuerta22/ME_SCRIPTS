path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"

train <- read.csv(paste0(path, "train.csv"),sep = ";")



respuesta <- "Customer.Lifetime.Value"
aux <- colnames(train)[which(!colnames(train) %in% c(respuesta))]
#aux <- colnames(train)[lapply(train, is.numeric) == TRUE]
#aux <- aux[-which(aux == respuesta)]
explicativas <- paste0(aux, collapse = " + ")
modelo <- paste0(respuesta, " ~ ", explicativas)

hist(train$Customer.Lifetime.Value)

aaa <- glm(modelo, family = poisson(link = "log") , data = train)

summary(aaa)

ml1 <- lm(modelo, data=train)

library(MASS)
ml1step <- stepAIC(ml1, direction="both", trace=0)

summary(ml1step)

plot(ml1step)

anova(ml1step)


modelo.reducido <- update(ml1step, . ~ . - Location.CodeUrban)
summary(modelo.reducido)
plot(modelo.reducido)

