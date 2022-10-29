path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"

train <- read.csv(paste0(path, "train.csv"),sep = ";")

test <- read.csv(paste0(path, "test.csv"), sep = ";")


numeriques<-which(sapply(train,is.numeric))

categoriques <- which(sapply(train,is.character))

cat <- colnames(train)[categoriques]

#--------------------------------- ANCOVA -------------------------------------------------------------------


respuesta <- "Total.Claim.Amount"
hist(train[,respuesta], main = paste0("Histograma de ", respuesta), xlab = respuesta)

aux <- colnames(train)[which(!colnames(train) %in% c(respuesta,"Months.Since.Last.Claim", "Customer.Lifetime.Value", "Coverage", "EmploymentStatus", "Policy.Type", "Education", "Vehicle.Class", "Vehicle.Size"))]
explicativas <- paste0(aux, collapse = " + ")
modelo <- paste0(respuesta, " ~ ", explicativas)

ml1 <- lm(modelo, data=train)

summary(ml1)

library(MASS)
ml1step <- stepAIC(ml1, direction="both", trace=0)

summary(ml1step)

plot(ml1step)

#-------------------------------------- Performance del model ANCOVA --------------------------------------------------

actual <- test[, respuesta]

prediccio <- predict(ml1step, test, type = "response")

performance <- cor(actual, prediccio)

