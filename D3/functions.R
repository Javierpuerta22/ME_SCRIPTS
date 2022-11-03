#------- arxiu per exportar funcions ---------

#---------- funcion descriptiva univariante ---------

#X es un dataframe cualquiera
#install.packages("viridis")
library(viridisLite)

info_uni <- function(X){
  #Inicializaciones
  tamano_letra <- 1
  
  for (var in c(1:ncol(X))){
    
    name <- colnames(X[var])
    print(paste0(var, ": Informacio sobre la variable ", name), sep = "\n")
    
    if (class(X[,var]) == "character"){
      
      tabla = table(X[var])
      print(paste0("Nombre de modalitats : ", length(tabla)),sep = "\n")
      print("Taula de frequencies :",sep = "\n")
      print(prop.table(tabla))
      barplot(tabla, main = paste0("Barplot de ", name), cex.names = par("cex.axis"), col= viridis(10))
      pie(tabla, cex= tamano_letra, main=paste("Pie de", name), col = viridis(10))
    }
    else{
      hist(X[,var], main = paste0("Histograma de ", name),col=viridis(18), xlab= name)
      boxplot(X[,var], horizontal=TRUE, main=paste("Boxplot de",name), col = viridis(18))
      print("Estadistics de la variable :", sep = "\n")
      print(summary(X[var]))
      print(paste("Desviacio tipica: ", sd(X[, var], na.rm=TRUE)),sep = "\n")
      print(paste("Variancia: ", sd(X[, var], na.rm=TRUE)/mean(X[, var], na.rm=TRUE)), sep = "\n")
      
    }
  }
 
  
  
}

#---------- función descriptiva bivariante -------

#X es un dataframe cualquiera
info_bi <- function(X){
  k <- 2
  for (var1 in c(1:ncol(X))){
    for (var2 in c(k:(ncol(X)))){
      if (k < (ncol(X)+1)){
      
      v1 <- X[,var1]
      v2 <- X[,var2]
      
      name1 <- colnames(X[var1])
      name2 <- colnames(X[var2])
      
      
      
      print(paste0("Informació sobre les variables : ",name1, "(",class(v1),") i ",name2, "(",class(v2),")" ))
      
      if (class(v1) == "character" & class(v2) == "character" ){
        tabla <- table(v1, v2)
        barplot(tabla, main = paste0("Barplot de ", name1, " vs ", name2), cex.names = 1, col= viridis(length(levels(factor(v1)))), legend.text = TRUE ,beside = TRUE)
        print(tabla)
        print(paste0("El test de chi^2 entre ", name1, " i ", name2, " val: "))
        print(chisq.test(v1, v2))
        
      }
      
      else if ((class(v1) == "numeric" | class(v1) == "integer" )& (class(v2) == "numeric" | class(v2) == "integer")){
        plot(v1, v2, col= viridis(length(levels(factor(v1)))))
        
        print(paste0("La correlació entre ", name1, " i ",name2 ," és de: ", cor(v1, v2)))
        print(paste0("La covariancia entre ", name1, " i ",name2 ," és de: ", cov(v1, v2)))
      }
      
      else{
        if (class(v1) == "character" & (class(v2) == "numeric" | class(v2) == "integer" )) {
          
          tabla2 <- tapply(v2, v1, mean)
          barplot(tabla2 ,main=paste("Mitjanes de ", name2," per categories de ", name1 ), col = viridis(length(levels(factor(v1)))) )        
          print(tabla2)
        }
        if (class(v2) == "character" & (class(v1) == "numeric" | class(v1) == "integer" )){
        tabla3 <- tapply(v1, v2, mean)
        barplot(tabla3, main = paste("Mitjanes de ", name1," per categories de ", name2 ), col = viridis(length(levels(factor(v2)))))
        print(tabla2)}
      }
      }
    }
    k <- k + 1
  }
}


# ----------------------- FUNCIÓN CREAR FÒRMULA DE MODELO ------------------------

#dd es un dataframe cualquiera 
#response es la variable respuesta
#no_expl es un vector con las variables que no se quieren

create_formula <- function(dd, response, no_expl){

  aux <- colnames(dd)[which(!colnames(dd) %in% c(response,no_expl))]
  explicativas <- paste0(aux, collapse = " + ")
  modelo <- paste0(response, " ~ ", explicativas)
  
  return(modelo)
}

# ---------------------- FUNCIÓN QUE DEVUELVE LAS VAR CAT O NUMÉRICAS DE UN DATAFRAME -------------------

#dd es un dataframe cualquiera
#Si nombre es TRUE devuelve vector con el nombre de las var numéricas

var_num <- function(dd, nombre = FALSE){
  numero <-which(sapply(dd,is.numeric))
  
  if (nombre == TRUE){
    var <- colnames(dd)[numero]
    return(var)
  }
  return(numero)
}

#dd es un dataframe cualquiera
#Si nombre es TRUE devuelve vector con el nombre de las var categóricas

var_cat <- function(dd, nombre = FALSE){
  numero <-which(sapply(dd,is.character))
  
  if (nombre == TRUE){
    var <- colnames(dd)[numero]
    return(var)
  }
  return(numero)
}

