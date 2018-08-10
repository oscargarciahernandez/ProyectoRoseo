library(stringr)
library(magrittr)
library(here)
library(dplyr)
library(ggplot2)

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
  
  Vviento_piloto<- unique(df[which(df$experimento=="piloto"),][c(3,8)])
  df %<>% mutate(Vviento_estandar = ifelse(df$porcentaje==100,10.341669,
                                           ifelse(df$porcentaje==90,9.357483,
                                                  ifelse(df$porcentaje==80,8.326538,
                                                         ifelse(df$porcentaje==70,7.293694,
                                                                ifelse(df$porcentaje==60,6.040875,
                                                                       ifelse(df$porcentaje==50,5.474545,NA)))))))
  
  
  df%<>% mutate(wind_power_est = 0.5*1.2*0.27*0.45*(df$Vviento_estandar)^3,
                TSR_est = RPM*2*pi*r/60/df$Vviento_estandar,
                cp_est = watts/wind_power_est)
  
  return(df)
  
}
ploteo_experimento_estandar<- function(datos,grados){
  df<-datos
  select_groups <- function(data, groups) {
    data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]
  }
  
  group_number<-length(attr(group_by(df,experimento,angulo), "group"))
  for (groups_ind in 1:group_number) {
    
    xx<- df %>% group_by(.,experimento,angulo) %>% select_groups(groups_ind)
    nombre_1<-as.character(xx$experimento[1])
    nombre_2<-as.character(xx$angulo[1])
    nombre<-paste(nombre_1,nombre_2,sep = "_")
    
    
    percentaje_number<-length(attr(group_by(xx,porcentaje), "group") )
    xx_percentaje<-list()
    for (per in 1:percentaje_number) {
      xx_perc<- xx %>% group_by(.,porcentaje) %>% select_groups(per)
      xx_perc<-cbind(xx_perc$cp_est,xx_perc$TSR_est,xx_perc$Vviento_estandar)
      colnames(xx_perc)<- c("cp","TSR", "Vviento")
      xx_percentaje[[per]]<- xx_perc
      
    }
    
    
    
    
    ##lambda_cp es una tabla de dos columnas (cp,lambda) 
    lambda_Cp<- xx_percentaje
    lambda_Cp_clean<-list()
    for(j in 1:length(lambda_Cp)){
      cp_lmb<- lambda_Cp[[j]]
      cp_lmb<-cp_lmb[order(cp_lmb[,2]),]
      
      
      TSR_1<-cp_lmb[,2]
      Cp_1<-cp_lmb[,1]
      
      V_tsr<- seq(0.05,max(TSR_1),by=max(TSR_1)/8)
      TSR_2<- vector()
      Cp_2<- vector()
      
      for(i in 1:length(V_tsr)){
        
        Cp_2[i]<- Cp_1[which.min(abs(TSR_1-V_tsr[i]))]
        TSR_2[i]<-TSR_1[which.min(abs(TSR_1-V_tsr[i]))]
        
      }
      validacion<-cbind(unique(Cp_2),unique(TSR_2))
      validacion_1<- validacion[1:which.max(validacion[,1]),]
      validacion_2<- validacion[(which.max(validacion[,1])+1):length(validacion[,1]),]
      indeeex<- vector()
      rr<- 1 
      
      if(length(validacion_2)==2){
        validacion_2<- validacion_2
      }else{
        for (i in 1:length(validacion_2[,1])) {
          
          
            if(i==length(validacion_2[,1])){break}else{
              
              if(validacion_2[i,1] < validacion_2[(i+1),1]){
                indeeex[rr]<- as.numeric(i) 
                rr<-rr+1
               
              
            }
            
          }
        }
        
      }
  
      
      if(length(indeeex)==0){
        validacion_2<- validacion_2
      }else{
        validacion_2<-validacion_2[-indeeex,]
      }
      
      clean_table<-rbind(validacion_1,validacion_2)
      
      
      lambda_Cp_clean[[j]]<- clean_table
      
    }
    
    
    
    
    
    
    dir.create(paste0("C:/TFG/pruebaslaboratorio/graficos_estandar_fit",grados,"/"))
    
    jpeg(paste0("C:/TFG/pruebaslaboratorio/graficos_estandar_fit",grados,"/",nombre,".jpeg"))
    lambda_Cp<- lambda_Cp_clean
    colores<- c("orange","red","blue","dodgerblue4","purple","black")
    pch_dif<-c(0:5)
    
    for(i in 1:length(lambda_Cp)){
      #en caso de que sea mejor añadir el origen
      #x<- c(0,lambda_Cp[[i]][,2])
      #y<- c(0,lambda_Cp[[i]][,1])
      x<- lambda_Cp[[i]][,2]
      y<- lambda_Cp[[i]][,1]
      fit5<-lm(y~poly(x,grados,raw=TRUE))
      xx <- seq(min(x),max(x), by=0.01)
      
      plot(NULL,xlim=c(0,2),
           ylim = c(0,0.10),cex=0.005, yaxt ="n",
           xlab = "TSR", ylab = "Cp", bty='L')
      par(new=T)
      lines(xx, predict(fit5, data.frame(x=xx)), col=colores[i],lwd=2)
      cp_max<-max(predict(fit5, data.frame(x=xx)))
      lambda_max<- xx[which.max(predict(fit5, data.frame(x=xx)))]
      points(x,y, pch= pch_dif[i])
      par(new=T)
    }
    
    axis(2, at=seq(0,0.1, by=0.02),las=2)
    V_viento<-sapply(xx_percentaje,"[",1,3)
    
    leyenda_veintos<-paste0(round(V_viento,digits = 2), " m/s")
    orden_leyenda<-cbind(V_viento,leyenda_veintos,pch_dif,colores)
    orden_leyenda<-orden_leyenda[order(as.numeric(orden_leyenda[,1]), decreasing = TRUE),]
    
    legend("topright", inset=c(0,0),orden_leyenda[,2],pch = as.numeric(orden_leyenda[,3]),
           text.col = orden_leyenda[,4],ncol = 1,cex = 1)
    
    dev.off()
    
  }
  
}
ploteo_experimento_lectura<- function(datos,grados){
  df<-datos
  select_groups <- function(data, groups) {
    data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]
  }
  
  group_number<-length(attr(group_by(df,experimento,angulo), "group"))
  for (groups_ind in 1:group_number) {
    
    xx<- df %>% group_by(.,experimento,angulo) %>% select_groups(groups_ind)
    nombre_1<-as.character(xx$experimento[1])
    nombre_2<-as.character(xx$angulo[1])
    nombre<-paste(nombre_1,nombre_2,sep = "_")
    
    
    percentaje_number<-length(attr(group_by(xx,porcentaje), "group") )
    xx_percentaje<-list()
    for (per in 1:percentaje_number) {
      xx_perc<- xx %>% group_by(.,porcentaje) %>% select_groups(per)
      xx_perc<-cbind(xx_perc$cp,xx_perc$TSR,xx_perc$`m/s`)
      colnames(xx_perc)<- c("cp","TSR", "Vviento")
      xx_percentaje[[per]]<- xx_perc
      
    }
    
    
    
    
    ##lambda_cp es una tabla de dos columnas (cp,lambda) 
    lambda_Cp<- xx_percentaje
    lambda_Cp_clean<-list()
    for(j in 1:length(lambda_Cp)){
      cp_lmb<- lambda_Cp[[j]]
      cp_lmb<-cp_lmb[order(cp_lmb[,2]),]
      
      
      TSR_1<-cp_lmb[,2]
      Cp_1<-cp_lmb[,1]
      
      V_tsr<- seq(0.05,max(TSR_1),by=max(TSR_1)/8)
      TSR_2<- vector()
      Cp_2<- vector()
      
      for(i in 1:length(V_tsr)){
        
        Cp_2[i]<- Cp_1[which.min(abs(TSR_1-V_tsr[i]))]
        TSR_2[i]<-TSR_1[which.min(abs(TSR_1-V_tsr[i]))]
        
      }
      validacion<-cbind(unique(Cp_2),unique(TSR_2))
      validacion_1<- validacion[1:which.max(validacion[,1]),]
      validacion_2<- validacion[(which.max(validacion[,1])+1):length(validacion[,1]),]
      indeeex<- vector()
      rr<- 1 
      if(length(validacion_2)==2){
        validacion_2<- validacion_2
      }else{
        for (i in 1:length(validacion_2[,1])) {
          
          
            if(i==length(validacion_2[,1])){break}else{
              
              if(validacion_2[i,1] < validacion_2[(i+1),1]){
                indeeex[rr]<- as.numeric(i) 
                rr<-rr+1
                
              
            }
            
          }
        }
        
      }
      
      if(length(indeeex)==0){
        validacion_2<- validacion_2
      }else{
        validacion_2<-validacion_2[-indeeex,]
      }
      
      clean_table<-rbind(validacion_1,validacion_2)
      
      
      lambda_Cp_clean[[j]]<- clean_table
      
    }
    
    
    
    
    
    
    dir.create(paste0("C:/TFG/pruebaslaboratorio/graficos_lectura_fit",grados,"/"))
    
    jpeg(paste0("C:/TFG/pruebaslaboratorio/graficos_lectura_fit",grados,"/",nombre,".jpeg"))
    lambda_Cp<- lambda_Cp_clean
    colores<- c("orange","red","blue","dodgerblue4","purple","black")
    pch_dif<-c(0:5)
    
    for(i in 1:length(lambda_Cp)){
      #en caso de que sea mejor añadir el origen
      #x<- c(0,lambda_Cp[[i]][,2])
      #y<- c(0,lambda_Cp[[i]][,1])
      x<- lambda_Cp[[i]][,2]
      y<- lambda_Cp[[i]][,1]
      fit5<-lm(y~poly(x,grados,raw=TRUE))
      xx <- seq(min(x),max(x), by=0.01)
      
      plot(NULL,xlim=c(0,2),
           ylim = c(0,0.60),cex=0.005, yaxt ="n",
           xlab = "TSR", ylab = "Cp", bty='L')
      par(new=T)
      lines(xx, predict(fit5, data.frame(x=xx)), col=colores[i],lwd=2)
      cp_max<-max(predict(fit5, data.frame(x=xx)))
      lambda_max<- xx[which.max(predict(fit5, data.frame(x=xx)))]
      points(x,y, pch= pch_dif[i])
      par(new=T)
    }
    
    axis(2, at=seq(0,0.6, by=0.1),las=2)
    V_viento<-sapply(xx_percentaje,"[",1,3)
    
    leyenda_veintos<-paste0(round(V_viento,digits = 2), " m/s")
    orden_leyenda<-cbind(V_viento,leyenda_veintos,pch_dif,colores)
    orden_leyenda<-orden_leyenda[order(as.numeric(orden_leyenda[,1]), decreasing = TRUE),]
    
    legend("topright", inset=c(0,0),orden_leyenda[,2],pch = as.numeric(orden_leyenda[,3]),
           text.col = orden_leyenda[,4],ncol = 1,cex = 1)
    
    dev.off()
    
  }
  
}
ploteo_experimento_media<- function(datos,grados){
  df<-datos
  df%<>% mutate(Vviento_media = (df$`m/s`+df$Vviento_estandar)/2)
  df%<>% mutate(wind_power_mean = 0.5*1.2*0.27*0.45*(df$Vviento_media)^3,
                TSR_mean = RPM*2*pi*r/60/df$Vviento_media,
                cp_mean = watts/wind_power_mean)
  select_groups <- function(data, groups) {
    data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]
  }
  
  group_number<-length(attr(group_by(df,experimento,angulo), "group"))
  for (groups_ind in 1:group_number) {
    
    xx<- df %>% group_by(.,experimento,angulo) %>% select_groups(groups_ind)
    nombre_1<-as.character(xx$experimento[1])
    nombre_2<-as.character(xx$angulo[1])
    nombre<-paste(nombre_1,nombre_2,sep = "_")
    
    
    percentaje_number<-length(attr(group_by(xx,porcentaje), "group") )
    xx_percentaje<-list()
    for (per in 1:percentaje_number) {
      xx_perc<- xx %>% group_by(.,porcentaje) %>% select_groups(per)
      xx_perc<-cbind(xx_perc$cp_mean,xx_perc$TSR_mean,xx_perc$Vviento_media)
      colnames(xx_perc)<- c("cp","TSR", "Vviento")
      xx_percentaje[[per]]<- xx_perc
      
    }
    
    
    
    
    ##lambda_cp es una tabla de dos columnas (cp,lambda) 
    lambda_Cp<- xx_percentaje
    lambda_Cp_clean<-list()
    for(j in 1:length(lambda_Cp)){
      cp_lmb<- lambda_Cp[[j]]
      cp_lmb<-cp_lmb[order(cp_lmb[,2]),]
      
      
      TSR_1<-cp_lmb[,2]
      Cp_1<-cp_lmb[,1]
      
      V_tsr<- seq(0.05,max(TSR_1),by=max(TSR_1)/8)
      TSR_2<- vector()
      Cp_2<- vector()
      
      for(i in 1:length(V_tsr)){
        
        Cp_2[i]<- Cp_1[which.min(abs(TSR_1-V_tsr[i]))]
        TSR_2[i]<-TSR_1[which.min(abs(TSR_1-V_tsr[i]))]
        
      }
      validacion<-cbind(unique(Cp_2),unique(TSR_2))
      validacion_1<- validacion[1:which.max(validacion[,1]),]
      validacion_2<- validacion[(which.max(validacion[,1])+1):length(validacion[,1]),]
      indeeex<- vector()
      rr<- 1 
      if(length(validacion_2)==2){
        validacion_2<- validacion_2
      }else{
        for (i in 1:length(validacion_2[,1])) {
          
          
          if(i==length(validacion_2[,1])){break}else{
            
            if(validacion_2[i,1] < validacion_2[(i+1),1]){
              indeeex[rr]<- as.numeric(i) 
              rr<-rr+1
              
              
            }
            
          }
        }
        
      }
      
      if(length(indeeex)==0){
        validacion_2<- validacion_2
      }else{
        validacion_2<-validacion_2[-indeeex,]
      }
      
      clean_table<-rbind(validacion_1,validacion_2)
      
      
      lambda_Cp_clean[[j]]<- clean_table
      
    }
    
    
    
    
    
    
    dir.create(paste0("C:/TFG/pruebaslaboratorio/graficos_media_fit",grados,"/"))
    
    jpeg(paste0("C:/TFG/pruebaslaboratorio/graficos_media_fit",grados,"/",nombre,".jpeg"))
    lambda_Cp<- lambda_Cp_clean
    colores<- c("orange","red","blue","dodgerblue4","purple","black")
    pch_dif<-c(0:5)
    
    for(i in 1:length(lambda_Cp)){
      #en caso de que sea mejor añadir el origen
      #x<- c(0,lambda_Cp[[i]][,2])
      #y<- c(0,lambda_Cp[[i]][,1])
      x<- lambda_Cp[[i]][,2]
      y<- lambda_Cp[[i]][,1]
      fit5<-lm(y~poly(x,grados,raw=TRUE))
      xx <- seq(min(x),max(x), by=0.01)
      
      plot(NULL,xlim=c(0,2),
           ylim = c(0,0.22),cex=0.005, yaxt ="n",
           xlab = "TSR", ylab = "Cp", bty='L')
      par(new=T)
      lines(xx, predict(fit5, data.frame(x=xx)), col=colores[i],lwd=2)
      cp_max<-max(predict(fit5, data.frame(x=xx)))
      lambda_max<- xx[which.max(predict(fit5, data.frame(x=xx)))]
      points(x,y, pch= pch_dif[i])
      par(new=T)
    }
    
    axis(2, at=seq(0,0.25, by=0.05),las=2)
    V_viento<-sapply(xx_percentaje,"[",1,3)
    
    leyenda_veintos<-paste0(round(V_viento,digits = 2), " m/s")
    orden_leyenda<-cbind(V_viento,leyenda_veintos,pch_dif,colores)
    orden_leyenda<-orden_leyenda[order(as.numeric(orden_leyenda[,1]), decreasing = TRUE),]
    
    legend("topright", inset=c(0,0),orden_leyenda[,2],pch = as.numeric(orden_leyenda[,3]),
           text.col = orden_leyenda[,4],ncol = 1,cex = 1)
    
    dev.off()
    
  }
  
}
procesador <- function(path_to_csv){
  
  path_to_txt <- str_replace(path_to_csv, pattern = 'csv',replacement = 'txt')
  
  path_to_speed <- str_split(path_to_csv, '/', simplify = TRUE)
  path_to_speed <- path_to_speed[-length(path_to_speed)]
  path_to_speed <- paste(path_to_speed, collapse='/')
  
  path_to_speed_mean <- paste(path_to_speed,"Vviento_mean.txt",sep = '/')
  path_to_speed <- paste(path_to_speed,"Vviento.txt",sep = '/')
  
  if(!file.exists(path_to_speed_mean)){
    
    speed_mean <- mean(read.table(path_to_speed)[,1])
    write.table(speed_mean,file = path_to_speed_mean)
  }else{
    speed_mean<- read.table(path_to_speed_mean)
    if(is.na(speed_mean)){
      speed_mean <- mean(read.table(path_to_speed)[,1])
      write.table(speed_mean,file = path_to_speed_mean) 
    } else{
      speed_mean <- speed_mean[1,1]
    }
  }
  
  
  
  rpm<-colMeans(as.data.frame(read.table(path_to_txt)))
  
  y<-read.table(path_to_csv, sep ="," )
  # El CSV que ofrece el BK es kk esta todo separado por comas
  volts_amps<- colMeans(as.data.frame(cbind(as.numeric(paste0(y[,1],".",y[,2])),
                                            as.numeric(y[,3]+y[,4]*10^(-4)))))
  
  ##introduzco las velocidades del experimento piloto: Vel
  processed_file <-as.data.frame( cbind(rpm[1],speed_mean, volts_amps[1], volts_amps[2]))
  names(processed_file)<- c("RPM","m/s","V", "A")
  
  return(processed_file)
}
ploteo_experimento_individual<- function(datos,grados,groups_ind){
  df<-datos
  df%<>% mutate(Vviento_media = (df$`m/s`+df$Vviento_estandar)/2)
  df%<>% mutate(wind_power_mean = 0.5*1.2*0.27*0.45*(df$Vviento_media)^3,
                TSR_mean = RPM*2*pi*r/60/df$Vviento_media,
                cp_mean = watts/wind_power_mean)
  
  Vviento_pared<- unique(df[which(df$experimento=="pared"),][c(3,8)])
  df %<>% mutate(Vviento_estandar_pared = ifelse(df$porcentaje==100,6.059878,
                                           ifelse(df$porcentaje==90,5.532072,
                                                  ifelse(df$porcentaje==80,4.973180,
                                                         ifelse(df$porcentaje==70,4.460114,
                                                                ifelse(df$porcentaje==60,3.869665,
                                                                       ifelse(df$porcentaje==50,3.074220,NA)))))))
  
  df%<>% mutate(wind_power_pared = 0.5*1.2*0.27*0.45*(df$Vviento_estandar_pared)^3,
                TSR_pared = RPM*2*pi*r/60/df$Vviento_estandar_pared,
                cp_pared = watts/wind_power_pared)
  
  select_groups <- function(data, groups) {
    data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]
  }
  
  group_number<-length(attr(group_by(df,experimento,angulo), "group"))
  
    
    xx<- df %>% group_by(.,experimento,angulo) %>% select_groups(groups_ind)
    nombre_1<-as.character(xx$experimento[1])
    nombre_2<-as.character(xx$angulo[1])
    nombre<-paste(nombre_1,nombre_2,sep = "_")
    dir.create(paste0("C:/TFG/pruebaslaboratorio/graficos_fit",grados,"_",nombre,"/"))
    
    percentaje_number<-length(attr(group_by(xx,porcentaje), "group") )
    xx_percentaje_media<-list()
    xx_percentaje_estandar<-list()
    xx_percentaje_estandar_pared<-list()
    xx_percentaje_lectura<-list()
    for (per in 1:percentaje_number) {
      xx_perc_m<- xx %>% group_by(.,porcentaje) %>% select_groups(per)
      xx_perc_m<-cbind(xx_perc_m$cp_mean,xx_perc_m$TSR_mean,xx_perc_m$Vviento_media)
      colnames(xx_perc_m)<- c("cp","TSR", "Vviento")
      xx_percentaje_media[[per]]<- xx_perc_m
      
      xx_perc_s<- xx %>% group_by(.,porcentaje) %>% select_groups(per)
      xx_perc_s<-cbind(xx_perc_s$cp_est,xx_perc_s$TSR_est,xx_perc_s$Vviento_estandar)
      colnames(xx_perc_s)<- c("cp","TSR", "Vviento")
      xx_percentaje_estandar[[per]]<- xx_perc_s
      
      xx_perc_sp<- xx %>% group_by(.,porcentaje) %>% select_groups(per)
      xx_perc_sp<-cbind(xx_perc_sp$cp_pared,xx_perc_sp$TSR_pared,xx_perc_sp$Vviento_estandar_pared)
      colnames(xx_perc_sp)<- c("cp","TSR", "Vviento")
      xx_percentaje_estandar_pared[[per]]<- xx_perc_sp
      
      xx_perc_l<- xx %>% group_by(.,porcentaje) %>% select_groups(per)
      xx_perc_l<-cbind(xx_perc_l$cp,xx_perc_l$TSR,xx_perc_l$`m/s`)
      colnames(xx_perc_l)<- c("cp","TSR", "Vviento")
      xx_percentaje_lectura[[per]]<- xx_perc_l
      
    }
    
    xx_data<- list(xx_percentaje_estandar,xx_percentaje_estandar_pared,
                   xx_percentaje_lectura,xx_percentaje_media)
    names(xx_data)<- c("xx_percentaje_estandar","xx_percentaje_estandar_pared",
                                  "xx_percentaje_lectura","xx_percentaje_media")
    for (ll in 1:length(xx_data)) {
      lambda_Cp<- xx_data[[ll]]
      lambda_Cp_clean<-list()
      for(j in 1:length(lambda_Cp)){
        cp_lmb<- lambda_Cp[[j]]
        cp_lmb<-cp_lmb[order(cp_lmb[,2]),]
        
        
        TSR_1<-cp_lmb[,2]
        Cp_1<-cp_lmb[,1]
        
        V_tsr<- seq(0.05,max(TSR_1),by=max(TSR_1)/8)
        TSR_2<- vector()
        Cp_2<- vector()
        
        for(i in 1:length(V_tsr)){
          
          Cp_2[i]<- Cp_1[which.min(abs(TSR_1-V_tsr[i]))]
          TSR_2[i]<-TSR_1[which.min(abs(TSR_1-V_tsr[i]))]
          
        }
        validacion<-cbind(unique(Cp_2),unique(TSR_2))
        validacion_1<- validacion[1:which.max(validacion[,1]),]
        validacion_2<- validacion[(which.max(validacion[,1])+1):length(validacion[,1]),]
        indeeex<- vector()
        rr<- 1 
        if(length(validacion_2)==2){
          validacion_2<- validacion_2
        }else{
          for (i in 1:length(validacion_2[,1])) {
            
            
            if(i==length(validacion_2[,1])){break}else{
              
              if(validacion_2[i,1] < validacion_2[(i+1),1]){
                indeeex[rr]<- as.numeric(i) 
                rr<-rr+1
                
                
              }
              
            }
          }
          
        }
        
        if(length(indeeex)==0){
          validacion_2<- validacion_2
        }else{
          validacion_2<-validacion_2[-indeeex,]
        }
        
        clean_table<-rbind(validacion_1,validacion_2)
        
        
        lambda_Cp_clean[[j]]<- clean_table
        
      }
      
      
      jpeg(paste0("C:/TFG/pruebaslaboratorio/graficos_fit",grados,"_",nombre,"/",names(xx_data)[[ll]],".jpeg"))
      lambda_Cp<- lambda_Cp_clean
      colores<- c("orange","red","blue","dodgerblue4","purple","black")
      pch_dif<-c(0:5)
      ###establecemos xlim e ylim
      lim_y<- (max(unlist(lapply(xx_data[[ll]],"[",,1))) + (0.02))
      lim_x<- (max(unlist(lapply(xx_data[[ll]],"[",,2)))+ (0.2))
      
      
      for(i in 1:length(lambda_Cp)){
        #en caso de que sea mejor añadir el origen
        #x<- c(0,lambda_Cp[[i]][,2])
        #y<- c(0,lambda_Cp[[i]][,1])
        x<- lambda_Cp[[i]][,2]
        y<- lambda_Cp[[i]][,1]
        fit5<-lm(y~poly(x,grados,raw=TRUE))
        xx <- seq(min(x),max(x), by=0.01)
        
        plot(NULL,xlim=c(0,lim_x),
             ylim = c(0,lim_y),cex=0.005, yaxt ="n",
             xlab = "TSR", ylab = "Cp", bty='L')
        par(new=T)
        lines(xx, predict(fit5, data.frame(x=xx)), col=colores[i],lwd=2)
        cp_max<-max(predict(fit5, data.frame(x=xx)))
        lambda_max<- xx[which.max(predict(fit5, data.frame(x=xx)))]
        points(x,y, pch= pch_dif[i])
        par(new=T)
      }
      
      axis(2, at=seq(0,0.25, by=0.05),las=2)
      V_viento<-sapply(xx_data[[ll]],"[",1,3)
      
      leyenda_veintos<-paste0(round(V_viento,digits = 2), " m/s")
      orden_leyenda<-cbind(V_viento,leyenda_veintos,pch_dif,colores)
      orden_leyenda<-orden_leyenda[order(as.numeric(orden_leyenda[,1]), decreasing = TRUE),]
      
      legend("topright", inset=c(0,0),orden_leyenda[,2],pch = as.numeric(orden_leyenda[,3]),
             text.col = orden_leyenda[,4],ncol = 1,cex = 1)
      
      dev.off()
      }
}
ajuste_RPM_Resistencia<- function(df){
  group_number<-length(attr(group_by(df,experimento,angulo,porcentaje), "group"))
  
  
  lista_rpm_resistencia<- list()
  nombres_lista<- vector()
  titulos_grafico<- vector()
  for (grupos in 1:group_number) {
    grupos_rpm_resistencia<- df %>% group_by(.,experimento,angulo,porcentaje) %>% select_groups(grupos)
    
    tabla_resistencia_rpm<- as.data.frame(cbind(as.numeric(grupos_rpm_resistencia$RPM),as.data.frame(grupos_rpm_resistencia$resistencia)))
    colnames(tabla_resistencia_rpm)<- c("RPM", "Omhnios")
    
    nombre_tabla<- unique(paste(grupos_rpm_resistencia$experimento,grupos_rpm_resistencia$angulo,grupos_rpm_resistencia$porcentaje,sep = "_"))
    if(is.na(grupos_rpm_resistencia$angulo)){
      titulo_graph<- unique(paste0(grupos_rpm_resistencia$experimento,"-",grupos_rpm_resistencia$porcentaje,"%"))
    }else{
      titulo_graph<- unique(paste0(grupos_rpm_resistencia$experimento,"-",grupos_rpm_resistencia$angulo,"º-",grupos_rpm_resistencia$porcentaje,"%"))
    }
    titulos_grafico[grupos]<- titulo_graph
    lista_rpm_resistencia[[grupos]]<- tabla_resistencia_rpm
    nombres_lista[grupos]<- nombre_tabla
  }
  
  ordenando<- function(ordenando){
    return(ordenando[order(as.numeric(as.character(ordenando$Omhnios))),] ) 
  }
  lista_rpm_resistencia_ordenada<-lapply(lista_rpm_resistencia, ordenando)
  
  for (pruebas in 1:length(lista_rpm_resistencia_ordenada)) {
    dir.create(paste0("C:/TFG/pruebaslaboratorio/graficos_RPM_Resistencia/"))
    
    jpeg(paste0("C:/TFG/pruebaslaboratorio/graficos_RPM_Resistencia/",nombres_lista[pruebas],".jpeg"))
    x<-as.numeric(as.character(lista_rpm_resistencia_ordenada[[pruebas]][,2]))
    y<- as.numeric(as.character(lista_rpm_resistencia_ordenada[[pruebas]][,1]))
    m<-nls(y~a*x/(b+x))
    #get some estimation of goodness of fit
    cor(y,predict(m))
    
    plot(NULL,xlim=c(0,max(x)),
         ylim = c(0,max(y)),cex=0.005, yaxt ="n",
         xlab = "Omhnios", ylab = "RPM", bty='L')
    par(new=T)
    lines(x,predict(m),lty=2,col="red",lwd=1)
    points(x,y, pch= 1)
    par(new=T)
    text(max(x)/2,max(y)/2,labels=paste("Correlación = ",as.character(round(cor(y,predict(m)),3))))
    axis(2, at=seq(0,max(y), by=round(max(y)/10,1)),las=2)
    title(main = "Gráfica RPM-Resistencia", sub = paste0("Experimento = ",titulos_grafico[pruebas]))
    dev.off()
    
  }
}



    ##lambda_cp es una tabla de dos columnas (cp,lambda) 
   
    
    
    
    
    
    
  
    
    
  
  

