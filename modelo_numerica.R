path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"

dd <- read.csv(paste0(path, "database_pre.csv"),sep = ";")

## 80% of the sample size
smp_size <- floor(0.8 * nrow(dd))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dd)), size = smp_size)

train <- dd[train_ind, ]
test <- dd[-train_ind, ]

train[5717:5717,]

train <- train[-c(5717,1975,6253),]

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
ml1step <- stepAIC(aaa, direction="both", trace=0)

summary(ml1step)

plot(ml1step)

anova(ml1step)


modelo.reducido <- update(ml1step, . ~ . - Location.CodeUrban)
summary(modelo.reducido)
plot(modelo.reducido)

