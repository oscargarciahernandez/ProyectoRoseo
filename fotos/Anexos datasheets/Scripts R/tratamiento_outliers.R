library(outliers)
outliers_prueba<-ajuste_RPM_Resistencia(df)
RPM_outliers<-sapply(outliers_prueba, "[",,1)
Resistencia_outliers<-sapply(outliers_prueba, "[",,2)
Resistencia_outliers<-sapply(Resistencia_outliers, function(x) as.numeric(as.character(x)))
chisq_outlier<-t(sapply(RPM_outliers, chisq.out.test))
alternative_outlier<- chisq_outlier[,2]
alternative_outlier<-str_extract_all(alternative_outlier,"\\d+")
make_outlier<- function(x){
 
  return( paste(as.numeric(x[1]),as.numeric(x[2]), sep = "."))
}
alternative_outlier<- sapply(alternative_outlier,make_outlier)
Eliminar_outlier<- function(lista_rpms, vector_outliers){
  difff<- abs(lista_rpms[length(lista_rpms)]-as.numeric(vector_outliers))
if(difff< 1){
  ret<-lista_rpms[-length(lista_rpms)]
}else{ret<- lista_rpms}
 return(ret) 
}
RPM_sin_outliers<- lapply(RPM_outliers, Eliminar_outlier, vector_outliers=alternative_outlier)

tabla_sin_outliers<- function(resistencia,rpms){
  if(length(resistencia)==length(rpms)){
   tabla<-cbind(rpms,resistencia)
  }else{
    tabla<- cbind(rpms, resistencia[-length(resistencia)])
  }
  return(tabla)
}
tablas_sin_ot<-mapply(tabla_sin_outliers, rpms=RPM_sin_outliers, resistencia=Resistencia_outliers)

eliminar_decreasing<-function(validacion){
  indeeex<- vector()
  rr<- 1 
  
  for (i in 1:length(validacion[,1])) {
    if(i==length(validacion[,1])){break}else{
      
      if(validacion[i+1,1] < validacion[(i),1]){
        indeeex[rr]<- as.numeric(i+1) 
        rr<-rr+1
        
        
      }
      
    }
  }
  
  if(length(indeeex)==0){
    validacion<- validacion
  }else{
    validacion<-validacion[-indeeex,]
  }
  
  return(validacion)
}

tablas_sin_outliers_ni_decreasing<-lapply(tablas_sin_ot, eliminar_decreasing)
 