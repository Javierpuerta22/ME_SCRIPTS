#------- arxiu per exportar funcions de descriptiva ---------

#---------- funcion descriptiva univariable ---------


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