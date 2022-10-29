library(ROCR)

path <- "C:/Users/pelot/Desktop/ME_SCRIPTS/"

train <- read.csv(paste0(path, "train.csv"),sep = ";")
test <- read.csv(paste0(path, "test.csv"),sep = ";")

respuesta <- "Income"

#-------------------------------- ANOVA -------------------------------------------------------------------

attach(train)

anova <- aov(Income~ Coverage + Education + EmploymentStatus + Location.Code + Vehicle.Size, data = train )

summary(anova)

plot(anova)

actually <- test[, respuesta]

prediction <- predict(anova, test, type = "response")

#--------------------------- Rendiment de l'anova --------------------------------------------------------

cor(actually, prediction)
