library(stringr)
library(magrittr)
library(here)
library(dplyr)

source(here('funcion_procesado.R'))
source(here('df_mutate.R'))

data_path<- here('data/')
archivos <- list.files(data_path, pattern = 'csv',
                        recursive = TRUE, full.names = TRUE)
r<-0.27/2 #radio del cacharro

lista_procesada<-lapply(archivos, procesador)
column_names <- names(lista_procesada[[1]])
df <- data.frame(matrix(unlist(lista_procesada), 
                        nrow=length(lista_procesada), 
                        byrow=T),stringsAsFactors=FALSE)
colnames(df) <- column_names
df<- cbind(archivos,df)







df %<>% mutate(experimento= factor( 
                 ifelse(str_detect(archivos, 'concentrador'), 'concentrador',
                 ifelse(str_detect(archivos, 'piloto'), 'piloto','pared')
                 ) 
                 )
                 )
df %<>% mutate(angulo=  str_extract(archivos,pattern = "concentrador/[0-9]+") ) %>% 
  mutate(angulo =  factor(str_remove(angulo,pattern = "concentrador/") ),
         porcentaje=str_extract(archivos, pattern = "prueba_[0-9]+")) %>% 
  mutate(porcentaje=factor(str_remove(porcentaje,pattern = "prueba_"))) %>% 
  mutate(resistencia=str_extract(archivos, pattern = "[0-9]+.csv")) %>% 
  mutate(resistencia=factor(str_remove(resistencia,pattern = ".csv")), 
         watts = V*A,
         wind_power = 0.5*1.2*0.27*0.45*(`m/s`)^3,
         TSR = RPM*2*pi*r/60/`m/s`,
         cp = watts/wind_power)


ggplot(df, aes(x=TSR, y = cp)) +
 geom_point()+
  facet_wrap(~experimento+angulo)



