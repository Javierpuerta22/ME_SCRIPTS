dd <- read.csv("C:/Users/pelot/Desktop/ME_SCRIPTS/database_pre.csv", sep = ";")

dd_mod <- dd


var_fact = c("State", "Coverage", "Education", "EmploymentStatus", "Location.Code", "Marital.Status", "Policy.Type", "Vehicle.Class")

dd_mod[, var_fact] <- lapply(dd_mod[, var_fact], as.factor)

colnames(dd_mod) <- c("ST", "CLV", "Resp.", "COV", "EDUC", "EMPS", "GEN", "INC", "LOCC", "MARS", "MPA", "MSLC", "MSPI", "NOP", "PT", "TCA", "VC", "VS")

#Cambiar etiquetas
levels(dd_mod$ST) <- c("AR", "CAL", "NEV", "OR", "WASH")
levels(dd_mod$Cov.) <- c("BAS", "EXT", "PREM")
levels(dd_mod$EDUC) <- c("BACH", "COLL", "DOCT", "HSOB", "MAST")
levels(dd_mod$EMPS) <- c("DIS", "EMP", "ML", "RET", "UNEMP")
levels(dd_mod$LOCC) <- c("RUR", "SUBU", "URB")
levels(dd_mod$MARS) <- c("DIV", "MAR", "SING")
levels(dd_mod$PT) <- c("CORP", "PERS", "SPEC")
levels(dd_mod$VC) <- c("4D", "LUX", "LUXS", "SPO", "SUV", "2D")
levels(dd_mod$VS) <- c("L", "M", "S")

write.table(dd_mod, file = "database_pre.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
