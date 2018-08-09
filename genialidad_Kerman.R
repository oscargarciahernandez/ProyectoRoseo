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

df<- df_mutate(df)


















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



#### solventar el problema de la mala medicion del viento utilizando las velocidades del viento
#### de la prueba piloto. 
Vviento_piloto<- unique(df[which(df$experimento=="piloto"),][c(3,8)])
df %<>% mutate(Vviento_estandar = ifelse(df$porcentaje==100,10.341669,
                                         ifelse(df$porcentaje==90,9.357483,
                                                ifelse(df$porcentaje==80,8.326538,
                                                       ifelse(df$porcentaje==70,7.293694,
                                                              ifelse(df$porcentaje==60,6.040875,
                                                                     ifelse(df$porcentaje==50,5.474545,NA)))))))


df%<>% mutate(wind_power_est = 0.5*1.2*0.27*0.45*(df$Vviento_estandar)^3,
              TSR_est = RPM*2*pi*r/60/df$Vviento_estandar,
              cp_est = watts_est/wind_power_est)

df%<>%select(.,-watts_est)
