# ==============================================================================
#  INTRODUCTION TO EXPERIMENTAL DESIGNS
# Author(s):     Karina Gibert i Sergi RamÃ­rez
#                        IDEAI (c)
# Date:               29 November 2022
# Description:   This script is intended to give tips regarding 
#                experimental design
#
# ==============================================================================
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
# Load the databases
data("ToothGrowth")

# ==============================================================================
# Randomizer
## Perform a t-test
ToothGrowth_ttest <- t.test(len ~ supp, data = ToothGrowth) #t.test comparacion medias

# Tidy ToothGrowth_ttest
broom::tidy(ToothGrowth_ttest) #saca el output del t.test mas limpio

# ------------------------------------------------------------------------------
# Replication

ToothGrowth %>% 
  count(supp, dose)

# ------------------------------------------------------------------------------
# Blocking

# Create a boxplot with geom_boxplot()
ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_boxplot()

# Create ToothGrowth_aov
ToothGrowth_aov <- aov(len ~ factor(dose) + supp, data = ToothGrowth) #analisis variancia 

# Examine ToothGrowth_aov with summary()
summary(ToothGrowth_aov)

# ==============================================================================
# Power & Sample Size Calculations #Cuanto asignar a cada grupo sabiendo cuanto me quiero equivocar (en porcentaje)
library(pwr)

# Calculate power #Ens diu k n ha de ser de 100
pwr.t.test(n = 100, 
           d = 0.35,
           sig.level = 0.10,
           type = "two.sample", 
           alternative = "two.sided",
           power = NULL)

# Calculate sample size
pwr.t.test(n = NULL, 
           d = 0.25, 
           sig.level = 0.05, 
           type = "one.sample", alternative = "greater", 
           power = 0.8)

# We eliminate the objects of the previous sections
rm(ToothGrowth, ToothGrowth_aov, ToothGrowth_ttest)

# ==============================================================================
# INTRODUCTION TO LENDING CLUB DATA
## https://www.kaggle.com/datasets/adarshsng/lending-club-loan-data-csv?resource=download&select=loan.csv
path <- '/Users/danis.p./Desktop/ME_SCRIPTS/'
## fichero <- 'loan.csv'
## lending <- read.csv(paste0(path, fichero), stringsAsFactors = TRUE)

# Realizamos un random de lending ya que tenemos mucha informaciÃ³n
## set.seed(06071994)
## index <- sample(1:nrow(lending), size = 0.15*nrow(lending), replace = FALSE)
## lending <- lending[index, ]
## rm(index);gc()
## save(lending, file = paste0(path, "lendingClub.RData"))
lending <- get(load(paste0(path, "database_pre.csv")))

# Examine the variables with glimpse()
glimpse(lending)

lending %>% summarize(median(loan_amnt), #Esta libreria permite agilizar el proceso
                      mean(int_rate))

# Use ggplot2 to build a bar chart of purpose
ggplot(lending, aes(purpose)) +
  geom_bar() +
  coord_flip()

# Use recode() to create the new purpose_recode variable
lending$purpose_recode <- lending$purpose %>% recode( 
  "credit_card" = "debt_related", 
  "debt_consolidation" = "debt_related",
  "medical" = "debt_related",
  "car" = "big_purchase", 
  "major_purchase" = "big_purchase", 
  "vacation" = "big_purchase",
  "moving" = "life_change", 
  "small_business" = "life_change", 
  "wedding" = "life_change",
  "house" = "home_related", 
  "home_improvement" = "home_related")

# ==============================================================================
# How does loan purpose affect amount funded?
## Build a linear regression model, purpose_recode_model
purpose_recode_model <- lm(funded_amnt ~ purpose_recode, data = lending)

# Examine results of purpose_recode_model
summary(purpose_recode_model)

# ------------------------------------------------------------------------------
# Get anova results and save as purpose_recode_anova
purpose_recode_anova <- anova(purpose_recode_model)

# Print purpose_recode_anova
purpose_recode_anova

# Examine class of purpose_recode_anova
class(purpose_recode_anova)

# ==============================================================================
# Which loan purpose mean is different?
## Use aov() to build purpose_aov
purpose_aov <- aov(funded_amnt ~ purpose_recode, data = lending)

## Conduct Tukey's HSD test to create tukey_output
tukey_output <- TukeyHSD(purpose_aov, "purpose_recode", conf.level = 0.95) #mirar combinaciones entre las variables (ya k intervalos de confianza quedan separados)

# Tidy tukey_output to make sense of the results
tidy(tukey_output)

# ==============================================================================
# Multiple Factor Experiments
## Use aov() to build purpose_emp_aov
purpose_emp_aov <- aov(funded_amnt ~ purpose_recode + emp_length, data = lending)

## Print purpose_emp_aov to the console
purpose_emp_aov

## Call summary() to see the p-values
summary(purpose_emp_aov)

# ==============================================================================
# Model validation
## Examine the summary of int_rate
summary(lending$int_rate)

## Examine int_rate by grade
lending %>% 
  group_by(grade) %>% 
  summarize(mean = mean(int_rate), var = var(int_rate), median = median(int_rate))

## Make a boxplot of int_rate by grade
ggplot(lending, aes(x = grade, y = int_rate)) +
  geom_boxplot()

## Use aov() to create grade_aov plus call summary() to print results
grade_aov <- aov(int_rate ~ grade, data = lending)
summary(grade_aov)

{
  # For a 2x2 grid of plots:
  par(mfrow = c(2, 2))
  
  # Plot grade_aov
  plot(grade_aov)
  
  # For a 1x1 grid plot
  par(mfrow = c(1, 1))
}

## Bartlett's test for homogeneity of variance
bartlett.test(int_rate ~ grade, data = lending)

# Conduct the Kruskal-Wallis rank sum test
kruskal.test(int_rate ~ grade, data = lending)

# We eliminate the objects of the previous sections
rm(grade_aov, purpose_aov, purpose_emp_aov, purpose_recode_model, lending)
gc();

# ==============================================================================
# Interaction plots:
## make this example reproducible
set.seed(671994)

## create data frame
data <- data.frame(gender = rep(c("Male", "Female"), each = 30),
                   exercise = rep(c("None", "Light", "Intense"), each = 10, times = 2),
                   weight_loss = c(runif(10, -3, 3), runif(10, 0, 5), runif(10, 5, 9),
                                   runif(10, -4, 2), runif(10, 0, 3), runif(10, 3, 8)))

## view first six rows of data frame
head(data)

## fit the two-way ANOVA model
model <- lm(weight_loss ~ gender * exercise, data = data) #genero femenino como variable base

## view the model output
summary(model) 
jtools::summ(model)

# ANOVA model
anovaModel <- aov(model)
summary(anovaModel) #nos dice k hacer ejercicio y ser hombre o mujer no tiene relacion

# Create effect plot
effect_plot(model, pred = gender, interval = TRUE, partial.residuals = TRUE)
effect_plot(model, pred = exercise, interval = TRUE, partial.residuals = TRUE)

# Create interaction plot
#interactions:: interact_plot(model, pred = factor(gender), modx = factor(exercise),  interval = TRUE, int.type = "confidence", int.width = .8)
interaction.plot(
  x.factor = data$exercise,
  trace.factor = data$gender,
  response = data$weight_loss,
  fun = median,
  ylab = "Weight loss after six weeks",
  xlab = "Exercise type",
  trace.label = "Gender",
  col = c("#0198f9", "#f95801"),
  lyt = 1,
  lwd = 3
)

# Homework: 
## Implementation interaction plot with the packages interactions
### https://interactions.jacob-long.com/

# ==============================================================================
# Reference: 
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
# https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html
# ==============================================================================