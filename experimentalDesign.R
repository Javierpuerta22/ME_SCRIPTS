# We load the necessary libraries
list.of.packages = c("agricolae", "wooldridge", "ggplot2", 
                     "broom", "dplyr", "pwr", "jtools", "interactions") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

# ==============================================================================

path <- "/Users/liza/Desktop/UNIVER/2.1/ME/ME_SCRIPTS/"
setwd("/Users/liza/Desktop/UNIVER/2.1/ME/ME_SCRIPTS/D3")
dd <- read.table("database_pre.csv",header=T, sep=";");


# Examinem les variables amb glimpse()
glimpse(dd) 

dd %>% summarize(median(dd$TCA), 
                          mean(dd$INC))

# Fem servir ggplot2 per construir un bar chart de  Vehicle.Class (VC)
ggplot(dd, aes(VC)) +
  geom_bar() +
  coord_flip()

# Fem servir recode() per crear una variable VC_recode
dd$VC_recode <- dd$VC %>% recode( 
  "LUXS" = "LUXE", 
  "LUX" = "LUXE")

# ==============================================================================
# Com afecta Vehicle.Class(VC) a Customer.Lifetime.Value (CLV)?
# Construïm una model de regressió lineal, VC_recode_model
VC_recode_model <- lm(CLV ~ VC_recode, data = dd)

# Examinem resultats de VC_recode_model
summary(VC_recode_model)

# ------------------------------------------------------------------------------
# Obtenim resultats del anova i els guardem com VC_recode_anova
VC_recode_anova <- anova(VC_recode_model)
# Veiem que existen diferencies significatives ja que els valors són inferiors a 0.05

# Imprimim VC_recode_anova
VC_recode_anova

# Examinem class de VC_recode_anova
class(VC_recode_anova)

# ==============================================================================
# Quina mitjana de Vehicle.Class és diferent?
## Fem servir aov() per construir VC_aov
VC_aov <- aov(CLV ~ VC_recode, data = dd)

## TExecutem ukey's HSD test
tukey_output <- TukeyHSD(VC_aov, "VC_recode", conf.level = 0.95)

# Observem els resultats
tidy(tukey_output)

# ==============================================================================
# Multiple Factor Experiments
## Usem aov() per construir VC_COV_aov on COV és Coverage.
VC_COV_aov <- aov(CLV ~ VC_recode + COV, data = dd)

## Imprimim VC_COV_aov
VC_COV_aov

## Executem summary() per veure els p-values
summary(VC_COV_aov)

# ==============================================================================
# Validació del model
summary(dd$INC)

## Examinem Income per Vehicle.Size
dd %>% 
  group_by(EDUC) %>% 
  summarize(mean = mean(INC), var = var(INC), median = median(INC))

## Fem boxplot de Income per Education
ggplot(dd, aes(x = EDUC, y = INC)) +
  geom_boxplot()

## Fem servir aov()per crear grade_aov i observem els resultats amb summary() 
EDUC_aov <- aov(INC ~ EDUC, data = dd)
summary(EDUC_aov)

{
par(mfrow = c(2, 2))

plot(EDUC_aov)

par(mfrow = c(1, 1))
}

## Bartlett's test per la homogenitat de la variança
bartlett.test(INC ~ EDUC, data = dd)

# Kruskal-Wallis rank sum test
kruskal.test(INC ~ EDUC, data = dd)

# ==============================================================================
# Interaction plots:

## fit the two-way ANOVA model
model <- lm(INC ~ GEN * EMPS , data = dd) 

## view the model output
summary(model) 
jtools::summ(model)

# ANOVA model
anovaModel <- aov(model)
summary(anovaModel)

# Create effect plot
effect_plot(model, pred = GEN, interval = TRUE, partial.residuals = TRUE)
effect_plot(model, pred = EMPS, interval = TRUE, partial.residuals = TRUE)

# Create interaction plot
#interactions:: interact_plot(model, pred = factor(gender), modx = factor(exercise),  interval = TRUE, int.type = "confidence", int.width = .8)
interaction.plot(
  x.factor = dd$EMPS,
  trace.factor = dd$GEN,
  response = dd$INC,
  fun = median,
  ylab = "Income",
  xlab = "Employment Status",
  trace.label = "Gender",
  col = c("#0198f9", "#f95801"),
  lyt = 1,
  lwd = 3
)

