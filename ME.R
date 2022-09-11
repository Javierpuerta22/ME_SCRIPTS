path <- "C:/Users/pelot/Desktop/ME_SCRIPTS"

dd <- read.csv(paste0(path, "database.csv"),sep = ",")

dd <- dd[-(8100:9134),]      #filas vacias

#eliminar columnas que no queremos
dd <- dd[, c(-1, -7, -16, -19, -21, -20)]


na_en_tabla <- apply(is.na(dd), 2, sum)

na_en_tabla



#sampling de 3000 aleatorias

rand_df <- dd[sample(nrow(dd), size = 3000),]

summary(rand_df)

summary(rand_df$Gender)

#proporciones

a = table(dd$Gender)

prop.table(a)

class(dd$Number.of.Policies)

#dataframe con numericas solo

clases <- lapply(dd, class)      #lista con las clases de cada variable

numericdd <- subset.data.frame(dd, drop = FALSE, select = which(clases == "numeric" | clases == "integer"))

pairs(numericdd)

#funcion univariable


info_uni <- function(X, nom){
  print(paste0("le toca la info a la variable ", nom))
  
  if (class(X) == "character"){
    tabla = table(X)
    print(prop.table(tabla))
  }
  else{
    hist(X, main = paste0("Histograma de ", nom))
    summary(X)
  }
  
  
}

#univariable simple

for (i in c(1:22)){
  info_uni(dd[,i], colnames(dd)[i])
}

summary(numericdd)





