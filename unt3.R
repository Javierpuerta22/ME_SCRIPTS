install.packages("forecast")
library(forecast)
library(ggplot2)
library(readxl)
library(httr)

path <-  '/Users/danis.p./Desktop/ME_SCRIPTS/D3/'
data <- read.csv(paste0(path,"Dataframe temporal.csv"),sep=";")
meses <- as.numeric(unlist(strsplit(data$DATE.TOTALSA, split = "M"))[c(F, T, F)]) #con[c(F,T)] nos permite ir alternando entre false y true y conseguimos los que son True "un truco" 
anyo <- as.numeric(unlist(strsplit(data$DATE.TOTALSA, split = "M"))[c(T, F, F)])

data$meses <- meses
data$anyo <- anyo
df
