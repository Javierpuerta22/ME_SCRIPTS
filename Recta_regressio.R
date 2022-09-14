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
