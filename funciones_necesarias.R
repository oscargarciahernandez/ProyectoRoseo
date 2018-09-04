library(stringr)
library(magrittr)
library(here)
library(dplyr)
library(ggplot2)
library(minpack.lm)


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
    
    
    
    
    
    
    dir.create(paste0(here(),"/graficos_estandar_fit",grados,"/"))
    
    #jpeg(paste0(here(),"/graficos_estandar_fit",grados,"/",nombre,".jpeg"))
    tiff(paste0(here(),"/graficos_estandar_fit",grados,"/",nombre,".tiff"), width = 7, height =7, units = 'in', res = 300)
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
    
    
    
    
    
    
    dir.create(paste0(here(),"/graficos_lectura_fit",grados,"/"))
    
    #jpeg(paste0(here(),"/graficos_lectura_fit",grados,"/",nombre,".jpeg"))
    tiff(paste0(here(),"/graficos_lectura_fit",grados,"/",nombre,".tiff"), width = 7, height =7, units = 'in', res = 300)
    
    
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
    
    
    
    
    
    
    dir.create(paste0(here(),"/graficos_media_fit",grados,"/"))
    
    #jpeg(paste0(here(),"/graficos_media_fit",grados,"/",nombre,".jpeg"))
    tiff(paste0(here(),"/graficos_media_fit",grados,"/",nombre,".tiff"), width = 7, height =7, units = 'in', res = 300)
    
    
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
select_groups <- function(data, groups) {
  data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]
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
    dir.create(paste0(here(),"/graficos_fit",grados,"_",nombre,"/"))
    
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
      
      
      #jpeg(paste0(here(),"/graficos_fit",grados,"_",nombre,"/",names(xx_data)[[ll]],".jpeg"))
      tiff(paste0(here(),"/graficos_fit",grados,"_",nombre,"/",names(xx_data)[[ll]],".tiff"), width = 7, height =7, units = 'in', res = 300)
      
      
      
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
    dir.create(paste0(here(),"/graficos_RPM_Resistencia/"))
    
    #jpeg(paste0(here(),"/graficos_RPM_Resistencia/",nombres_lista[pruebas],".jpeg"))
    tiff(paste0(here(),"/graficos_RPM_Resistencia/",nombres_lista[pruebas],".tiff"), width = 7, height =7, units = 'in', res = 300)
    
    
    
    
    
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
  return(lista_rpm_resistencia_ordenada)
}
ajuste_RPM_Resistencia_so<- function(df,tabla_sinout){
  group_number<-length(attr(group_by(df,experimento,angulo,porcentaje), "group"))
  r<- (-2)
  
  
  lista_rpm_resistencia<- list()
  nombres_lista<- vector()
  titulos_grafico<- vector()
  tabladenombres<- matrix(-31,ncol = 3, nrow = group_number)
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
    tabladenombres[grupos,1]<-as.character(grupos_rpm_resistencia$experimento[1])
    tabladenombres[grupos,2]<- as.character(grupos_rpm_resistencia$angulo[1])
    tabladenombres[grupos,3]<- as.character(grupos_rpm_resistencia$porcentaje[1])
  }
  
  ordenando<- function(ordenando){
    return(ordenando[order(as.numeric(as.character(ordenando$Omhnios))),] ) 
  }
  lista_rpm_resistencia_ordenada<-lapply(lista_rpm_resistencia, ordenando)
  
  
  lista_coef<-list()
  for (pruebas in 1:length(lista_rpm_resistencia_ordenada)) {
    dir.create(paste0(here(),"/graficos_RPM_Resistencia_sinout/"))
    
    #jpeg(paste0(here(),"/graficos_RPM_Resistencia/",nombres_lista[pruebas],".jpeg"))
    tiff(paste0(here(),"/graficos_RPM_Resistencia/",nombres_lista[pruebas],".tiff"), width = 7, height =7, units = 'in', res = 300)
    
    
    
    x<-as.numeric(as.character(lista_rpm_resistencia_ordenada[[pruebas]][,2]))
    x_so<-tabla_sinout[[pruebas]][,2]
    y<- as.numeric(as.character(lista_rpm_resistencia_ordenada[[pruebas]][,1]))
    y_so<-tabla_sinout[[pruebas]][,1]
    
    #dat<-as.data.frame(cbind(tabla_sinout[[pruebas]][,2],tabla_sinout[[pruebas]][,1]))
    #names(dat)<- c("x","y")
    #f <- function(x,a,b) {a * exp(b * x)}
    #fm0 <- nls(log(y) ~ log(f(x, a, b)),dat, start = c(a = 1, b = 1))
    
    #nls(y_so ~ f(x_so, a, b), start = coef(fm0))
    
    
    m<-nls(y~a*x/(b+x))
    m_so_1<-function(y_so,x_so){
      return(nls(y_so~a*x_so/(b+x_so)))
    }
    m_so_2<-function(y_so,x_so){
      return(nlsLM(y_so~a*x_so/(b+x_so)))
    }
    m_so<-tryCatch(m_so_1(y_so,x_so), error=function(e) m_so_2(y_so,x_so))

    
    
    
    coefa_so<-coef(m_so)[1]
  coefb_so<- coef(m_so)[2]
  x_so_seq<- seq(0,8000,by=1)
  y_so_seq<- coefa_so*x_so_seq/(coefb_so+x_so_seq)
  
  coefa <-coef(m)[1]
  coefb <- coef(m)[2]
  x_seq<- seq(0,8000,by=1)
  y_seq<- coefa*x_seq/(coefb+x_seq)
    
    
    
    
    
    plot(NULL,xlim=c(0,8000),
         ylim = c(0,(max(y_so_seq)+(max(y_so_seq))/5)),cex=0.005, yaxt ="n",
         xlab =  expression(paste("Resistencia (",Omega,")")), ylab = "Velocidad Angular (RPM)", bty='L')
    par(new=T)
    #lines(x,predict(m),lty=2,col="red",lwd=1)
    lines(x_seq,y_seq,lty=2, col="red",lwd=1)
    par(new=T)
    #lines(x_so,predict(m_so),lty=2,col="blue",lwd=1)
    lines(x_so_seq,y_so_seq,lty=2,col="blue",lwd=1)
    
    points(x_so,y_so, pch= 4, col="blue")
    
   
    
     x_dif<-length(setdiff(x, x_so))
    y_dif<-length(setdiff(round(y,r),round(y_so,r)))
  
    
    while(x_dif != y_dif){
      y_dif<-length(setdiff(round(y,r),round(y_so,r)))
      r<-r+1}
  
    
    if(x_dif==y_dif){
      points(setdiff(x, x_so),setdiff(round(y,r),round(y_so,r)), pch=20, col="red", cex=1.5)
      
    }
    
    
    
    
    
    
    subtitle_nom<- paste0("Experimento = ",titulos_grafico[pruebas])
    maintitle<- paste0("Gráfica Velocidad Angular-Resistencia \n",subtitle_nom)
    par(new=T)
    axis(2, at=seq(0,(max(y_so_seq)+(max(y_so_seq))/5), by=round((max(y_so_seq)+(max(y_so_seq))/5)/10,-1)),las=2)
    title(main = maintitle)
    
    R_sinVA<- paste0("Regresión sin VA"," (Correlación= ",as.character(round(cor(y_so,predict(m_so)),3)),")")
    R_conVA<- paste0("Regresión sin VA"," (Correlación= ",as.character(round(cor(y,predict(m)),3)),")")
    

    legend("right", inset=c(0,0),
           legend = c(R_sinVA,R_conVA,"Valores comunes", "Valores atípicos "),
           pch = c(NA,NA, 4,20),
           lty = c(2,2,NA,NA), 
           col = c("blue","red","black","red"),ncol = 1,cex = 1)
    
        dev.off()
        
        correlacion_sino<-cor(y_so,predict(m_so))
        correlacion_cono<-cor(y,predict(m))
        if(correlacion_sino>= correlacion_cono){
          aa<- coefa_so
          bb<- coefb_so
        }else{
          aa<- coefa
          bb<-coefb
        }
        
        coef_tabla<- cbind(as.character(tabladenombres[pruebas,1]),as.character(tabladenombres[pruebas,2]),
                           as.character(tabladenombres[pruebas,3]),aa,bb)  
        names(coef_tabla)<-c("Experimento","Angulo","Porcentaje","a","b")
        
      lista_coef[[pruebas]]<-coef_tabla
        
  }

 return(lista_coef) 
  
}
add_coef<-function(df,coeficientes_RPM){
  df %<>% mutate(coef_a= -31,coef_b=-31)
  vector_logico<- list()
  for (i in 1:length(coeficientes_RPM[,1])) {
    if(coeficientes_RPM[i,1]=="concentrador"){
      vector_logico[[i]]<- c(coeficientes_RPM[i,1]==df$experimento & coeficientes_RPM[i,2]==df$angulo & coeficientes_RPM[i,3]==df$porcentaje)
      
    }else{
      vector_logico[[i]]<- c(coeficientes_RPM[i,1]==df$experimento & coeficientes_RPM[i,3]==df$porcentaje)
      
    }
  }
  for (j in 1:length(vector_logico)) {
    df %<>% mutate(coef_a=replace(coef_a, vector_logico[[j]],as.numeric(as.character( coeficientes_RPM[j,4]))),
                   coef_b=replace(coef_b, vector_logico[[j]],as.numeric(as.character( coeficientes_RPM[j,5]))))
  }
  
  df %<>% mutate(RPM_regresion= (as.numeric(as.character(coef_a)) *as.numeric(as.character(resistencia)))/(as.numeric(as.character(coef_b))+as.numeric(as.character(resistencia))))
  df %<>% mutate(diff_rpm=RPM-RPM_regresion, TSR_regresion_est = RPM_regresion*2*pi*r/60/Vviento_estandar,TSR_regresion_lectura = RPM_regresion*2*pi*r/60/`m/s`)
  }


    

##### ploteos de TSR_CP empleando los valores de TSR de la regresion   
    
ploteo_experimento_estandar_RPM_regresion<- function(datos,grados){
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
    if(is.na(nombre_2)){
      nombre_grafica<- nombre_1
    }else{
      nombre_grafica<-paste(nombre_1," ",nombre_2,"º",sep = "")
    }
    
    percentaje_number<-length(attr(group_by(xx,porcentaje), "group") )
    xx_percentaje<-list()
    for (per in 1:percentaje_number) {
      xx_perc<- xx %>% group_by(.,porcentaje) %>% select_groups(per)
      xx_perc<-cbind(xx_perc$cp_est,xx_perc$TSR_regresion_est,xx_perc$Vviento_estandar)
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
    
    
    
    
    
    
    dir.create(paste0(here(),"/graficos_RPMreg_estandar_fit",grados,"/"))
    
    #jpeg(paste0(here(),"/graficos_RPMreg_estandar_fit",grados,"/",nombre,".jpeg"))
    tiff(paste0(here(),"/graficos_RPMreg_estandar_fit",grados,"/",nombre,".tiff"), width = 7, height =7, units = 'in', res = 300)
    
    
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
    
    subtitle_nom<- paste0("Experimento = ",nombre_grafica)
    maintitle<- paste0("Gráfica CP-TSR \n",subtitle_nom)
    title(main = maintitle)
    
    dev.off()
    
  }
  
}
ploteo_experimento_lectura_RPM_regresion<- function(datos,grados){
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
      xx_perc<-cbind(xx_perc$cp,xx_perc$TSR_regresion_lectura,xx_perc$`m/s`)
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
    
    
    
    
    
    
    dir.create(paste0(here(),"/graficos_RPMreg_lectura_fit",grados,"/"))
    
    #jpeg(paste0(here(),"/graficos_RPMreg_lectura_fit",grados,"/",nombre,".jpeg"))
    tiff(paste0(here(),"/graficos_RPMreg_lectura_fit",grados,"/",nombre,".tiff"), width = 7, height =7, units = 'in', res = 300)
    
    
    
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
ploteo_experimento_media_RPM_regresion<- function(datos,grados){
  df<-datos
  df%<>% mutate(Vviento_media = (df$`m/s`+df$Vviento_estandar)/2)
  df%<>% mutate(wind_power_mean = 0.5*1.2*0.27*0.45*(df$Vviento_media)^3,
                TSR_mean = RPM_regresion*2*pi*r/60/df$Vviento_media,
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
    
    
    
    
    
    
    dir.create(paste0(here(),"/graficos_RPMreg_media_fit",grados,"/"))
    
    #jpeg(paste0(here(),"/graficos_RPMreg_media_fit",grados,"/",nombre,".jpeg"))
    tiff(paste0(here(),"/graficos_RPMreg_media_fit",grados,"/",nombre,".tiff"), width = 7, height =7, units = 'in', res = 300)
    
    
    
    
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

    
## Ploteos CP_TSR uniendo los puntos de CP_max

ploteo_experimento_estandar_RPM_regresion_CPmax<- function(datos,grados){
  df<-datos
  select_groups <- function(data, groups) {
    data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]
  }
  
  lista_Cpmax_total<-list()
  group_number<-length(attr(group_by(df,experimento,angulo), "group"))
  for (groups_ind in 1:group_number) {
    
    xx<- df %>% group_by(.,experimento,angulo) %>% select_groups(groups_ind)
    nombre_1<-as.character(xx$experimento[1])
    nombre_2<-as.character(xx$angulo[1])
    nombre<-paste(nombre_1,nombre_2,sep = "_")
    if(is.na(nombre_2)){
      nombre_grafica<- nombre_1
    }else{
      nombre_grafica<-paste(nombre_1," ",nombre_2,"º",sep = "")
    }
    
    
    percentaje_number<-length(attr(group_by(xx,porcentaje), "group") )
    xx_percentaje<-list()
    for (per in 1:percentaje_number) {
      xx_perc<- xx %>% group_by(.,porcentaje) %>% select_groups(per)
      xx_perc<-cbind(xx_perc$cp_est,xx_perc$TSR_regresion_est,xx_perc$Vviento_estandar)
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
    
    
    
    
    
    
    dir.create(paste0(here(),"/graficos_Cpmax_RPMreg_estandar_fit",grados,"/"))
    
    #jpeg(paste0(here(),"/graficos_Cpmax_RPMreg_estandar_fit",grados,"/",nombre,".jpeg"))
    tiff(paste0(here(),"/graficos_Cpmax_RPMreg_estandar_fit",grados,"/",nombre,".tiff"), width = 7, height =7, units = 'in', res = 300)
    
    
    lambda_Cp<- lambda_Cp_clean
    colores<- c("orange","red","blue","dodgerblue4","purple","black")
    pch_dif<-c(0:5)
    lista_Cpmax<-list()
    for(i in 1:length(lambda_Cp)){
      x<- lambda_Cp[[i]][,2]
      y<- lambda_Cp[[i]][,1]
      fit5<-lm(y~poly(x,grados,raw=TRUE))
      xx <- seq(min(x),max(x), by=0.01)
      
      plot(NULL,xlim=c(0,2),
           ylim = c(0,0.10),cex=0.005, yaxt ="n",
           xlab = "TSR", ylab = "Cp", bty='L')
      par(new=T)
      lines(xx, predict(fit5, data.frame(x=xx)), col=colores[i],lwd=2)
      tabla_maxcp<-cbind(xx,predict(fit5, data.frame(x=xx)))
      Cp_max_point<-tabla_maxcp[which.max(tabla_maxcp[,2]),]
      lista_Cpmax[[i]]<- Cp_max_point
      par(new=T)
    }
    
    axis(2, at=seq(0,0.1, by=0.02),las=2)
    V_viento<-sapply(xx_percentaje,"[",1,3)
    
    leyenda_veintos<-paste0(round(V_viento,digits = 2), " m/s")
    orden_leyenda<-cbind(V_viento,leyenda_veintos,pch_dif,colores)
    orden_leyenda<-orden_leyenda[order(as.numeric(orden_leyenda[,1]), decreasing = TRUE),]
    
    legend("topright", inset=c(0,0),orden_leyenda[,2],pch = as.numeric(orden_leyenda[,3]),
           text.col = orden_leyenda[,4],ncol = 1,cex = 1)
    
    tabla_CPmax<-data.frame(matrix(unlist(lista_Cpmax), 
                      nrow=length(lista_Cpmax), 
                      byrow=T),stringsAsFactors=FALSE)
    tabla_CPmax<-rbind(tabla_CPmax)
    names(tabla_CPmax)<- c("TSR","Cp")
    tabla_CPmax<- tabla_CPmax[order(tabla_CPmax[,1]),]
    
    points(tabla_CPmax$TSR,tabla_CPmax$Cp, pch= 20,cex=2)
    y<- tabla_CPmax$Cp
    x<- tabla_CPmax$TSR
    
    fit_cp<-lm(y~poly(x,grados,raw=TRUE))
    
    
    
    m_so_1<-function(y_so,x_so){
      return(nls(y_so~a*x_so/(b+x_so), start = list(a=-100, b=-100)))
    }
    m_so_2<-function(y_so,x_so){
      return(nlsLM(y_so~a*x_so/(b+x_so), start = list(a=-100, b=-100)))
    }
    m_so<-tryCatch(m_so_1(y,x), error=function(e) m_so_2(y,x))
    a_m<- as.numeric(as.character(coef(m_so)[1]))
    b_m<-as.numeric(as.character(coef(m_so)[2]))
    y_m_so<- (a_m*xx)/(b_m+xx)
    
    
    #lines(xx, predict(fit_cp, data.frame(x=xx)), col="black",lwd=1,lty=2)
    #lines(xx,y_m_so, col="grey",lwd=1, lty=2)
    
    
    subtitle_nom<- paste0("Experimento = ",nombre_grafica)
    maintitle<- paste0("Gráfica CP-TSR \n",subtitle_nom)
    title(main = maintitle)
    
    dev.off()
    
    lista_Cpmax_total[[groups_ind]]<- tabla_CPmax
    
  }
  
  return(lista_Cpmax_total)
}


## Elaboracion Grafica Potencia_m/s y devuelve los coeficientes
##de la regresion. 
grafica_Potencia_V<-function(df,xlimite,ylimite){
select_groups <- function(data, groups) {
  data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]
}

group_number<-length(attr(group_by(df,experimento,angulo), "group"))
lista_watts_Vviento<- list()
nombres_expr<-vector()
for (groups_ind in 1:group_number) {
  
  xx<- df %>% group_by(.,experimento,angulo) %>% select_groups(groups_ind)
  nombre_1<-as.character(xx$experimento[1])
  nombre_2<-as.character(xx$angulo[1])
  nombre<-paste(nombre_1,nombre_2,sep = "_")

  
  percentaje_number<-length(attr(group_by(xx,porcentaje), "group") )
  xx_percentaje<-list()
  for (per in 1:percentaje_number) {
    xx_perc<- xx %>% group_by(.,porcentaje) %>% select_groups(per)
    xx_perc<-cbind(xx_perc$cp_est,xx_perc$watts,xx_perc$Vviento_estandar)
    colnames(xx_perc)<- c("cp","watts", "Vviento")
    xx_percentaje[[per]]<- xx_perc
    
  }
  lista_watts_Vviento[[groups_ind]]<- xx_percentaje
  nombres_expr[groups_ind]<- nombre
}

names(lista_watts_Vviento)<- nombres_expr

lista_watts_Vviento_max<-list()
for (i in 1:length(lista_watts_Vviento)) {
  
  df_watts_Vviento<- data.frame(matrix(unlist(lapply(lista_watts_Vviento[[i]],
                                                     function(x) x[which.max(x[,1]),2:3])),
                                       nrow=6, byrow=T))
  names(df_watts_Vviento)<- c("Watss","Vviento")
  lista_watts_Vviento_max[[i]]<- df_watts_Vviento
}
names(lista_watts_Vviento_max)<- nombres_expr

dir.create(paste0(here(),"/graficos_Potencia_V/"))

#jpeg(paste0(here(),"/graficos_Potencia_V/grafica_Potencia_V.jpeg"))
tiff(paste0(here(),"/graficos_Potencia_V/grafica_Potencia_V.tiff"), width = 7, height =7, units = 'in', res = 300)


colores<- c("orange","red","blue","dodgerblue4","purple","black")
pch_dif<-c(0:5)
correlacion<- vector()
lista_coef<-list()
for(i in 1:length(lista_watts_Vviento_max)){
  #en caso de que sea mejor añadir el origen
  #x<- c(0,lambda_Cp[[i]][,2])
  #y<- c(0,lambda_Cp[[i]][,1])
  x<- lista_watts_Vviento_max[[i]][,2]
  y<- lista_watts_Vviento_max[[i]][,1]
  fit_curva<-nls(y~b+a*x^3,start = list(a=0, b=0))
  xx <- seq(min(x),xlimite[2], by=0.1)
  
  plot(NULL,xlim=xlimite,
       ylim = ylimite,cex=0.005, yaxt ="n",
       xlab = "Velocidad del viento (m/s)", ylab = "Potencia (W)", bty='L')
  title(main= "Curvas de potencia")
  par(new=T)
  lines(xx, predict(fit_curva, data.frame(x=xx)), col=colores[i],lwd=1,lty=2)
  points(x,y, pch= pch_dif[i])
  par(new=T)
  correlacion[i]<- cor(y,predict(fit_curva))
  lista_coef[[i]]<- coef(fit_curva)
}

axis(2, at=seq(0,ylimite[2], by=round(ylimite[2]/7,0)),las=2)

titulos_graficos<-function(df){
  group_number<-length(attr(group_by(df,experimento,angulo,porcentaje), "group"))
  lista_rpm_resistencia<- list()
  nombres_lista<- vector()
  titulos_grafico<- vector()
  tabladenombres<- matrix(-31,ncol = 3, nrow = group_number)
  for (grupos in 1:group_number) {
    grupos_rpm_resistencia<- df %>% group_by(.,experimento,angulo,porcentaje) %>% select_groups(grupos)
    
    tabla_resistencia_rpm<- as.data.frame(cbind(as.numeric(grupos_rpm_resistencia$RPM),as.data.frame(grupos_rpm_resistencia$resistencia)))
    colnames(tabla_resistencia_rpm)<- c("RPM", "Omhnios")
    
    nombre_tabla<- unique(paste(grupos_rpm_resistencia$experimento,grupos_rpm_resistencia$angulo,grupos_rpm_resistencia$porcentaje,sep = "_"))
    
    if(is.na(grupos_rpm_resistencia$angulo)){
      titulo_graph<- unique(paste0(grupos_rpm_resistencia$experimento))
    }else{
      titulo_graph<- unique(paste0(grupos_rpm_resistencia$experimento,"-",grupos_rpm_resistencia$angulo,"º"))
    }
    titulos_grafico[grupos]<- titulo_graph
    lista_rpm_resistencia[[grupos]]<- tabla_resistencia_rpm
    nombres_lista[grupos]<- nombre_tabla
    tabladenombres[grupos,1]<-as.character(grupos_rpm_resistencia$experimento[1])
    tabladenombres[grupos,2]<- as.character(grupos_rpm_resistencia$angulo[1])
    tabladenombres[grupos,3]<- as.character(grupos_rpm_resistencia$porcentaje[1])
  }
  return(unique(titulos_grafico))
  
}
nombres_df<- titulos_graficos(df)


leyenda<- paste0(nombres_df," (Cor= ",round(correlacion,4),")")

legend("topleft",y.intersp = 0.75,seg.len = 0.9,
       bty="n", bg="transparent",inset=c(0,0),
       legend = leyenda,lty = c(2,2,2,2,2),lwd = c(2,2,2,2,2),col = colores[1:5],ncol = 1,cex = 1)
dev.off()

tabladenombress<-function(df){
  group_number<-length(attr(group_by(df,experimento,angulo), "group"))
  tabladenombres<- matrix(-31,ncol = 2, nrow = group_number)
  for (grupos in 1:group_number) {
    grupos_rpm_resistencia<- df %>% group_by(.,experimento,angulo) %>% select_groups(grupos)
    tabladenombres[grupos,1]<-as.character(grupos_rpm_resistencia$experimento[1])
    tabladenombres[grupos,2]<- as.character(grupos_rpm_resistencia$angulo[1])
  }
  return(as.data.frame(tabladenombres))
}

tablanombres<-tabladenombress(df)

coeficientes_P_V<-data.frame(matrix(unlist(lista_coef),nrow=5, byrow=T))

coef_tabla<- cbind(tablanombres,coeficientes_P_V) 
names(coef_tabla)<-c("Experimento","Angulo","a","b")

return(coef_tabla)

}



add_coef_P_V<-function(df,coeficientes_Curva_P_V){
  df %<>% mutate(coef_a_PV= -31,coef_b_PV=-31)
  vector_logico<- list()
  for (i in 1:length(coeficientes_Curva_P_V[,1])) {
    if(coeficientes_Curva_P_V[i,1]=="concentrador"){
      vector_logico[[i]]<- c(coeficientes_Curva_P_V[i,1]==df$experimento & coeficientes_Curva_P_V[i,2]==df$angulo)
      
    }else{
      vector_logico[[i]]<- c(coeficientes_Curva_P_V[i,1]==df$experimento)
      
    }
  }
  for (j in 1:length(vector_logico)) {
    df %<>% mutate(coef_a_PV=replace(coef_a_PV, vector_logico[[j]],as.numeric(as.character( coeficientes_Curva_P_V[j,3]))),
                   coef_b_PV=replace(coef_b_PV, vector_logico[[j]],as.numeric(as.character( coeficientes_Curva_P_V[j,4]))))
  }
  
  df %<>% mutate(watts_regresion= (as.numeric(as.character(coef_a_PV)) *as.numeric(as.character(Vviento_estandar)))+(as.numeric(as.character(coef_b))))
  
}


##funcion para plotear todas las graficas de Cp a todas las velocidades, comparando experimentos
ploteo_CPmax10<- function(datos,grados){
  df<-datos
  select_groups <- function(data, groups) {
    data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]
  }
  
  lista_Cpmax_total<-list()
  lista_Cps_nueva<-list()
  nombres_gra<- vector()
  group_number<-length(attr(group_by(df,experimento,angulo), "group"))
  for (groups_ind in 1:group_number) {
    
    xx<- df %>% group_by(.,experimento,angulo) %>% select_groups(groups_ind)
    percentaje_number<-length(attr(group_by(xx,porcentaje), "group") )
    
    nombre_1<-as.character(xx$experimento[1])
    nombre_2<-as.character(xx$angulo[1])
    nombre<-paste(nombre_1,nombre_2,sep = "_")
    
    if(is.na(nombre_2)){
      nombre_grafica<- nombre_1
    }else{
      nombre_grafica<-paste(nombre_1," ",nombre_2,"º",sep = "")
    }
    
    xx_percentaje<-list()
    for (per in 1:percentaje_number) {
      xx_perc<- xx %>% group_by(.,porcentaje) %>% select_groups(per)
      xx_perc<-cbind(xx_perc$cp_est,xx_perc$TSR_regresion_est,xx_perc$Vviento_estandar)
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
    
    lista_Cps_nueva[[groups_ind]]<- lambda_Cp_clean
    nombres_gra[groups_ind]<- nombre_grafica
    
  } 
  
  vector_nombre_vel<- c(10.34,5.47,6.04,7.29,8.33,9.36)
  
  
  for (porcentaje in 1:length(lista_Cps_nueva[[1]])) {
    
    lista_cpmsVmax<- lapply(lista_Cps_nueva,"[[",porcentaje)
    
    x_max<-max(sapply(lista_cpmsVmax, function(x) max(x[,2])))
    y_max<-max(sapply(lista_cpmsVmax, function(x) max(x[,1])))
    
    
    dir.create(paste0(here(),"/graficos_Cp_juntos/"))
    
    #jpeg(paste0(here(),"/graficos_Cp_juntos/",porcentaje,".jpeg"))
    tiff(paste0(here(),"/graficos_Cp_juntos/",porcentaje,".tiff"), width = 7, height =7, units = 'in', res = 300)
    colores<- c("orange","red","blue","dodgerblue4","purple")
    pch_dif<-c(0:5)
    for(i in 1:(length(lista_cpmsVmax))){
      x<- lista_cpmsVmax[[i]][,2]
      y<- lista_cpmsVmax[[i]][,1]
      fit5<-lm(y~poly(x,grados,raw=TRUE))
      xx <- seq(min(x),max(x), by=0.01)
      
      plot(NULL,xlim=c(0,(x_max+0.2)),
           ylim = c(0,(y_max+0.01)),cex=0.005, yaxt ="n",
           xlab = "TSR", ylab = "Cp", bty='L')
      par(new=T)
      lines(xx, predict(fit5, data.frame(x=xx)), col=colores[i],lwd=2)
      tabla_maxcp<-cbind(xx,predict(fit5, data.frame(x=xx)))
      par(new=T)
    }
    
    axis(2, at=unique(round(seq(0,(y_max+0.02), by=round((y_max/10),digits = 4)),digits = 2)),las=2)
    
    leyenda<-paste0(nombres_gra)
    
    
    legend("topright", inset=c(0,0),leyenda,
           text.col = colores,ncol = 1,cex = 1)
    
    titulo<- paste0("Comparación de gráficas Cp-TSR a ",vector_nombre_vel[porcentaje],"m/s")
    title(main = titulo)
    
    dev.off()
    
    
  }
  
} 





## funcion para plotear las velocidades en funcion de la resistencia en cada 
##experimento

RPM_por_porcentaje<-function(df){
  porcentaje_num<-c(50,60,70,80,90,100)
  for (j in 1:length(porcentaje_num)) {
    p<-filter(df,porcentaje==porcentaje_num[j])  
    
    
    select_groups <- function(data, groups) {
      data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]
    }
    
    group_number<-length(attr(group_by(p,experimento,angulo), "group"))
    lista_porcentaje<- list()
    for (groups_ind in 1:group_number) {
      xx<- p %>% group_by(.,experimento,angulo) %>% select_groups(groups_ind)
      lista_porcentaje[[groups_ind]]<- xx
    }
    lista_RPM_res<-lapply(lista_porcentaje, function(x) cbind(x$RPM_regresion,as.numeric(as.character(x$resistencia))))
    nombre_lista<-unlist(lapply(lapply(sapply(lista_porcentaje, function(x) cbind(as.character(x$experimento),as.character(x$angulo)))
                                       , "[",1,), function(x) paste(x[1],x[2],sep = "_")))
    names(lista_RPM_res)<- nombre_lista
    
    leyenda<- unlist(lapply(lapply(sapply(lista_porcentaje, function(x) cbind(as.character(x$experimento),as.character(x$angulo)))
                                   , "[",1,), function(x) ifelse(is.na(x[2]),x[1],paste(x[1]," ",x[2],"º",sep = ""))))
    
    
    
    
    
    
    y_lim<-  max(sapply(lista_RPM_res, function(x) max(x[,1])))
    
    
    colores<- c("orange","red","blue","dodgerblue4","purple")
    dir.create(paste0(here(),"/graficos_RPM_comparativa/"))
    
    #jpeg(paste0(here(),"/graficos_RPM_comparativa/",porcentaje_num[j],".jpeg"))
    tiff(paste0(here(),"/graficos_RPM_comparativa/",porcentaje_num[j],".tiff"), width = 7, height =7, units = 'in', res = 300)
    
    
    
     plot(NULL, xlim = c(0,8000), ylim = c(0,y_lim),yaxt ="n",
         xlab =  expression(paste("Resistencia (",Omega,")")), 
         ylab = "Velocidad Angular (RPM)", bty='L')
    
    
    for (i in 1:length(lista_RPM_res)) {
      x<- lista_RPM_res[[i]][,2]
      y<- lista_RPM_res[[i]][,1]
      m_so_1<-function(y_so,x_so){
        return(nls(y_so~a*x_so/(b+x_so),start = list(a=0,b=0)))
      }
      m_so_2<-function(y_so,x_so){
        return(nlsLM(y_so~a*x_so/(b+x_so),start = list(a=0,b=0)))
      }
      m<-tryCatch(m_so_1(y,x), error=function(e) m_so_2(y,x))  
      coefa <-coef(m)[1]
      coefb <- coef(m)[2]
      x_seq<- seq(0,8000,by=1)
      y_seq<- coefa*x_seq/(coefb+x_seq)
      par(new=T)
      lines(x_seq,y_seq,lty=2, col=colores[i],lwd=2)
      
      
    }
    subtitle_nom<- paste0("Experimento = ",porcentaje_num[j],"%")
    maintitle<- paste0("Gráfica comparativa Velocidad Angular-Resistencia \n",subtitle_nom)
    par(new=T)
    axis(2, at=seq(0,y_lim,by=round(y_lim/10,0)),las=2)
    title(main = maintitle)
    par(new=T)
    legend("bottomright", inset=c(0,0),leyenda,
           text.col = colores,ncol = 1,cex = 1)
    
    dev.off()
    
  }
  
  
  
}
