library(MASS)
library(RcmdrMisc)

path <-  '/Users/usuari/Desktop/2IA/ME/ME_SCRIPTS/'
data <- read.csv(paste0(path,"database_pre.csv"),sep=";")
train <- read.csv(paste0(path,"train.csv"),sep=";")
test <- read.csv(paste0(path,"test.csv"),sep=";")
#------------------------------------------------ auxiliars ------------------------------------------------------------
aux.train <- train
aux.test <- test

aux.train$Gender <- ifelse(train$Gender == "M", 0, 1) # Male = 0 / Female = 1
table(aux.train$Gender)

aux.test$Gender <- ifelse(test$Gender == "M", 0, 1) # Male = 0 / Female = 1
table(aux.test$Gender)

#----------------------------------------- Creació variables explicatives ------------------------------------------------------------------

respuesta <- "Gender"
hist(aux.train[,respuesta], main = paste0("Histograma de ", respuesta), xlab = respuesta)
aux <- colnames(aux.train)[which(!colnames(aux.train) %in% c(respuesta,"State","Customer.Lifetime.Value","Education","Response","Marital.Status","Policy.Type","Number.of.Policies","Months.Since.Last.Claim", "Coverage", "Location.Code", "Vehicle.Size"))]
explicativas <- paste0(aux, collapse = " + ")
modelo <- paste0(respuesta, " ~ ", explicativas)

#--------------------------------------------- Model binomial --------------------------------------------------------------

m1 <- glm(paste0("aux.train$",modelo) , family = binomial, data= train)
summary(m1)
plot(m1)


modelo <- stepwise(m1, direction='backward/forward', criterion='AIC')
plot(modelo)

summary(modelo)

# Variables significativas
sig.var <- summary(m1)$coeff[-1,4] < 0.01
names(sig.var)[sig.var == TRUE]

#------------------------------------------- Performance del model -----------------------------------------------------------

prediccio <- predict(modelo, test, type = "response")

prediccio <- ifelse(p22 > 0.5, 1,0)
taula <- table(aux.test$Gender, prediccio)
taula

accur <- sum(taula[1,1], taula[2,2])/sum(taula) *100
accur 







