train <- read.csv("C:/Users/pelot/Desktop/ME_SCRIPTS/train.csv", sep = ";")

train_mod <- train


var_fact = c("State", "Coverage", "Education", "EmploymentStatus", "Location.Code", "Marital.Status", "Policy.Type", "Vehicle.Class")

train_mod[, var_fact] <- lapply(train_mod[, var_fact], as.factor)

colnames(train_mod) <- c("ST", "CLV", "Resp.", "COV", "EDUC", "EMPS", "GEN", "INC", "LOCC", "MARS", "MPA", "MSLC", "MSPI", "NOP", "PT", "TCA", "VC", "VS")

#Cambiar etiquetas
levels(train_mod$ST) <- c("AR", "CAL", "NEV", "OR", "WASH")
levels(train_mod$Cov.) <- c("BAS", "EXT", "PREM")
levels(train_mod$EDUC) <- c("BACH", "COLL", "DOCT", "HSOB", "MAST")
levels(train_mod$EMPS) <- c("DIS", "EMP", "ML", "RET", "UNEMP")
levels(train_mod$LOCC) <- c("RUR", "SUBU", "URB")
levels(train_mod$MARS) <- c("DIV", "MAR", "SING")
levels(train_mod$PT) <- c("CORP", "PERS", "SPEC")
levels(train_mod$VC) <- c("4D", "LUX", "LUXS", "SPO", "SUV", "2D")
levels(train_mod$VS) <- c("L", "M", "S")

write.table(train_mod, file = "train_fact.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
