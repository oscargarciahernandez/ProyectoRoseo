df_mutate<-function(tabla_cruda){
  df<- tabla_cruda
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
  
  return(df)
  
}