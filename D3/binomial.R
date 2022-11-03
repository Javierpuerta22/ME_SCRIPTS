library(MASS)
library(RcmdrMisc)

path <-  'C:/Users/pelot/Desktop/ME_SCRIPTS/D3/'
data <- read.csv(paste0(path,"database_pre.csv"),sep=";")
train <- read.csv(paste0(path,"train.csv"),sep=";")
test <- read.csv(paste0(path,"test.csv"),sep=";")
source(paste0(path, "functions.R"))

#------------------------------------------------ auxiliars ------------------------------------------------------------
aux.train <- train
aux.test <- test

aux.train$Gender <- ifelse(train$Gender == "M", 0, 1) # Male = 0 / Female = 1
table(aux.train$Gender)

aux.test$Gender <- ifelse(test$Gender == "M", 0, 1) # Male = 0 / Female = 1
table(aux.test$Gender)

#----------------------------------------- Creació variables explicatives ------------------------------------------------------------------

respuesta <- "Gender"
no_queremos <- c("State","Customer.Lifetime.Value","Education","Response","Marital.Status","Policy.Type","Number.of.Policies","Months.Since.Last.Claim", "Coverage", "Location.Code", "Vehicle.Size")

modelo <- create_formula(aux.train, respuesta, no_queremos)

#--------------------------------------------- Model binomial --------------------------------------------------------------

m1 <- glm(paste0("aux.train$",modelo) , family = binomial, data= train)
summary(m1)
plot(m1)


ml1step <- stepwise(m1, direction='backward/forward', criterion='AIC')
summary(ml1step)
plot(ml1step)



# Variables significativas
sig.var <- summary(ml1step)$coeff[-1,4] < 0.01
names(sig.var)[sig.var == TRUE]

#------------------------------------------- Performance del model -----------------------------------------------------------

prediccio <- predict(ml1step, test, type = "response")

prediccio <- ifelse(prediccio > 0.5, 1,0)
taula <- table(aux.test$Gender, prediccio)
taula

error <- sum(taula[1,1], taula[2,2])/sum(taula) *100
error







