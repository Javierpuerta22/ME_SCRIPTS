#------- arxiu per exportar funcions de descriptiva ---------

#---------- funcion descriptiva univariante ---------

#X es un dataframe cualquiera

info_uni <- function(X){
  for (var in c(1:ncol(X))){
    
    print(paste0("Informació sobre la variable ", colnames(X[var])))
    
    if (class(X[,var]) == "character"){
      tabla = table(X[var])
      print(prop.table(tabla))
      barplot(tabla, main = paste0("Barplot de ", colnames(X[var])))
    }
    else{
      hist(X[,var], main = paste0("Histograma de ", colnames(X[var])))
      print(summary(X[var]))
    }
  }
 
  
  
}

info_uni(dd)

#---------- función descriptiva bivariante -------

#X es un dataframe cualquiera

info_bi <- function(X){
  k <- 2
  for (var1 in c(1:ncol(X))){
    for (var2 in c(k:ncol(X))){
      #Poner todo lo de descriptiva bivariable
      
      
      
    }
    k <- k + 1
  }
  
}