path <- "C:/Users/adria/IA/3r Quadri/ME/ME_SCRIPTS/"
dd <- read.csv(paste0(path,"database_pre.csv"),sep=";")

#https://fhernanb.github.io/libro_regresion/rls.html
#Tutorial de regressions

#Agafem les variables numeriques i veiem la correlació entre elles
clases <- lapply(dd, class)
numericdd <- subset.data.frame(dd, drop = FALSE, select = which(clases == "numeric" | clases == "integer"))

cor(numericdd[, 2:ncol(numericdd)])

#Asignem la variable resposta i calculem el model
respuesta = "Customer.Lifetime.Value"
aux <- colnames(numericdd)[which(! colnames(numericdd) %in% c("Months.Since.Policy.Inception", respuesta))]
explicativas <- paste0(aux, collapse = " + ")
modelo <- paste0(respuesta, " ~ ", explicativas)

ml1 <- lm(modelo, data=numericdd)
summary(ml1)

#Trobem el millor model a partir del generat anteriorment
library(MASS)
ml1step <- stepAIC(ml1, direction="both", trace=1)

summary(ml1step)
plot(ml1)




#------ recta logistica titanic ejemplo ----------
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




