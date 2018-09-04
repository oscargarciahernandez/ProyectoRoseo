library(stringr)
library(magrittr)
library(here)
library(dplyr)
  
ploteo_experimento<- function(datos,grados){
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
      for (i in 1:length(validacion_2[,1])) {
        
        if(length(validacion_2)==2){
          break
          
        }else{
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
    
    
    
    
    
    
    dir.create(paste0("C:/TFG/pruebaslaboratorio/graficos_fit",grados,"/"))
    
    jpeg(paste0("C:/TFG/pruebaslaboratorio/graficos_fit",grados,"/",nombre,".jpeg"))
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


