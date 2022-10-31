# -------------------------------------------- ARCHIVO CON EL MODELO ANOVA -----------------------------

path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/D3/"
train <- read.csv(paste0(path, "train.csv"),sep = ";")
test <- read.csv(paste0(path, "test.csv"),sep = ";")
source(paste0(path, "functions.R"))

respuesta <- "Income"
hist(train$Income)

#-------------------------------- ANOVA -------------------------------------------------------------------

attach(train)

cat <- var_cat(train, TRUE)

for (i in cat){
  train$i <- as.factor(train[, i])
}

anova <- aov(Income~ Education + EmploymentStatus + Location.Code + Vehicle.Size, data = train )
summary(anova)
plot(anova)

formula <- create_formula(train, respuesta, c(var_num(train, TRUE), "Vehicle.Class", "Marital.Status", "State","Response", "Gender","Coverage","Policy.Type"))

anova <- lm(formula, data = train)
summary(anova)
anova(anova)


#--------------------------- Rendiment de l'anova --------------------------------------------------------
actually <- test[, respuesta]
prediction <- predict(anova, test, type = "response")
performance <- cor(actually, prediction)

R2 <- 0.69 # valor R^2 del modelo
