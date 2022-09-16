#Archivo Main donde ir llamando funciones de otros archivos

path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"

dd <- read.csv(paste0(path, "database_pre.csv"),sep = ";")

clases <- lapply(dd, class)      #lista con las clases de cada variable

numericdd <- subset.data.frame(dd, drop = FALSE, select = which(clases == "numeric" | clases == "integer"))

na_en_tabla <- apply(is.na(dd), 2, sum)    #solo dice el numero de NA que hay en el database

na_en_tabla

cor(numericdd)


respuesta <- "Income"

aux <- colnames(numericdd)[which(!colnames(numericdd) %in% c(respuesta))]

explicativas <- paste0(aux, collapse = " + ")

model <- paste0(respuesta, " ~ ", explicativas)

ml1 <- lm(model, data = numericdd)

summary(ml1)

library(MASS)

ml1step <- MASS::stepAIC(ml1, direction = "both", trace = 1)

summary(ml1step)

plot(ml1)


#regresion logística -> devuelve la probabilidad de ponerse en 2 grupos, 0 o 1

train <- read.csv('http://idaejin.github.io/courses/R/data/titanic_train.csv',header=TRUE,row.names=1)
test <- read.csv('http://idaejin.github.io/courses/R/data/titanic_test.csv',header=TRUE,row.names=1)

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)

mod3 <-  glm(Survived ~ Pclass + Sex + Pclass:Sex + Age + SibSp, family = binomial(logit), data = train)
summary(mod3)

fitted.results <- predict(mod3,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))
      