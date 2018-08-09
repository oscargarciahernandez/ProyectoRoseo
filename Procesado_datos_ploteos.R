
source(here('funciones_necesarias.R'))

data_path<- here('data/')
archivos <- list.files(data_path, pattern = 'csv',
                       recursive = TRUE, full.names = TRUE)
r<-0.27/2 #radio del cacharro




##La funcion procesador entra dentro de cada carpeta de los datos de laboratorio y destripa
## toda la informacion creando un data frame con la informacion de V,A, Velocidad del viento 
## y la informacion del path de cada dato que da informacion de que experimento se trata. 

lista_procesada<-lapply(archivos, procesador)
column_names <- names(lista_procesada[[1]])
df <- data.frame(matrix(unlist(lista_procesada), 
                        nrow=length(lista_procesada), 
                        byrow=T),stringsAsFactors=FALSE)
colnames(df) <- column_names
df<- cbind(archivos,df)



##df_mutate es una funcion que trata los datos df, añadiendo los calculos pertinentes para
##calculo de potencia del viento, TSR, CP, etc. Hay que tener en cuenta que esta funcion añade
## unos calculos con la velocidad medida en cada experimento y luego tambien incluye los calculos
## empleando las velocidades del viento de la prueba piloto. Ya que la velocidad se modifica 
##demasiado al incluir la pared y el concentrador, por lo tanto no se puede tomar como velocidad d
## del viento para el ensayo. YO CREO¡  que incuimos error de esta manera. 

df<- df_mutate(df)


## ejecutando plote_experimento(df,grado) crea una carpeta dentro de pruebas laboratorio
## con las 5 graficas de todos los experimentos con xx grados de ajuste. 

ploteo_experimento(df,3)



