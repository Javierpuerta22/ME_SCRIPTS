# -------------------------------------------- ARCHIVO CON EL MODELO ANOVA -----------------------------

path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/D3/"
train <- read.csv(paste0(path, "train.csv"),sep = ";")
test <- read.csv(paste0(path, "test.csv"),sep = ";")
source(paste0(path, "functions.R"))

respuesta <- "Income"
hist(train$Income)

#-------------------------------- ANOVA -------------------------------------------------------------------

attach(train)

anova <- aov(Income~ Education + EmploymentStatus + Location.Code + Vehicle.Size, data = train )
summary(anova)
plot(anova)

actually <- test[, respuesta]

prediction <- predict(anova, test, type = "response")

#--------------------------- Rendiment de l'anova --------------------------------------------------------

performance <- cor(actually, prediction)
