#---------------------------------------------- ARCHIVO CON EL MODELO ANCOVA ---------------------------
library(MASS)

path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"
train <- read.csv(paste0(path, "train.csv"),sep = ";")
test <- read.csv(paste0(path, "test.csv"), sep = ";")
source(paste0(path, "functions.R"))

#-------------------------------------- ANCOVA -------------------------------------------------------------------


respuesta <- "Total.Claim.Amount"
hist(train[,respuesta], main = paste0("Histograma de ", respuesta), xlab = respuesta)
no_queremos <- c("Months.Since.Last.Claim", "Customer.Lifetime.Value", "Coverage", "EmploymentStatus", "Policy.Type", "Education", "Vehicle.Class", "Vehicle.Size")

modelo <- create_formula(train, respuesta, no_queremos)

ml1 <- lm(modelo, data=train)
summary(ml1)
plot(ml1)

ml1step <- stepAIC(ml1, direction="both", trace=0)
summary(ml1step)
plot(ml1step)

#-------------------------------------- Performance del model ANCOVA --------------------------------------------------

actual <- test[, respuesta]

prediccio <- predict(ml1step, test, type = "response")

performance <- cor(actual, prediccio)

