library(here)

fcn_input <- 'concentrador'


inputpaths2<- c("prueba_50/","prueba_60/","prueba_70/","prueba_80/",
                "prueba_90/","prueba_100/")

inputpath<-paste(data_path,fcn_input,inputpaths2, sep = '/')

data_path<- here('data/')
inputpath2 <- data_path
archivos2 <- list.files(inputpath2, pattern = 'csv', recursive = TRUE, full.names = TRUE)


archivos<- list.files(inputpath)
archivos[ str_detect(archivos, 'csv')] %>% str_remove('.csv') %>% as.numeric() %>% .[order(., decreasing = TRUE)]

resistencias<-list(resistencia_50,resistencia_60,resistencia_70,
                   resistencia_80,resistencia_90,
                   resistencia_100)


inputpaths_viento1<-"C:/TFG/pruebas laboratorio/"

inputpaths_viento2<- c("prueba_50/","prueba_60/","prueba_70/","prueba_80/",
                      "prueba_90/","prueba_100/")
inputpaths_viento<-paste0(inputpaths_viento1,inputpaths_viento2)

v_viento<-list()
for (i  in 1:6) {
  
  setwd(inputpaths_viento[i])
  rr<-as.data.frame(as.numeric(as.character(read.table("Vviento.txt")[,1])))
  rr<- rr[complete.cases(rr[,1]),1]
  
  v_viento[i]<-rr
}



Vel<-sapply(v_viento, mean)





experimento_piloto<-list()
for(j in 1:length(resistencias)){
  
  
  
  res_txt<-paste0(resistencias[[j]], ".txt")
  res_csv<-paste0(resistencias[[j]], ".csv")
  experimento<- list()
  for(i in 1:length(resistencias[[j]])){
    x<-colMeans(as.data.frame(read.table(res_txt[i])))
    #v<-as.data.frame(as.numeric(as.character(read.table("Vviento.txt")[,1])))
    #v<- v[complete.cases(v[,1]),1]
    #v<-mean(v)
    y<-read.table(res_csv[1], sep ="," )
    y<-read.table(res_csv[i], sep ="," )
    yy<- colMeans(as.data.frame(cbind(as.numeric(paste0(y[,1],".",y[,2])),
                                      as.numeric(y[,3]+y[,4]*10^(-4)))))
    
    ##introduzco las velocidades del experimento piloto: Vel
    z<-as.data.frame( cbind(x[1],Vel[j],yy[1],yy[2]))
    names(z)<- c("RPM","m/s","V", "A")
    experimento[[i]]<- z
  }
  experimento_frame<- matrix(-31,ncol  = 4, nrow = length(experimento))
  for(x in 1:length(experimento)){
    
    experimento_frame[x,]<- as.matrix(experimento[[x]])
    
  }
  experimento_frame<-as.data.frame(experimento_frame)
  names(experimento_frame)<- c("RPM","m/s","V", "A")
  
  watts<-vector()
  for (i in 1:length(experimento_frame[,1])) {
    watts[i]<- experimento_frame[i,3]*experimento_frame[i,4]
  }
  watts<-as.data.frame(watts)
  
  r<-0.27/2
  TSR<-as.data.frame(((experimento_frame$RPM*2*pi/60)*r)/experimento_frame$`m/s`)
  pviento<-0.5*1.2*0.27*0.45*(experimento_frame$`m/s`)^3
  cp<- as.vector(watts/pviento)
  
  experimento_frame<- cbind(experimento_frame[,1:4], watts, TSR, cp)
  names(experimento_frame)<- c("RPM","m/s","V", "A","Pot(W)", "TSR","Cp")
  
  
  experimento_piloto[[j]]<- experimento_frame
  
}



lambda_Cp<-list()
for(j in 1:length(experimento_piloto)){
  TSR_1<-experimento_piloto[[j]]$TSR
  Cp_1<-experimento_piloto[[j]]$Cp
  
  V_tsr<- seq(0.05,max(TSR_1),by=0.05)
  TSR_2<- vector()
  Cp_2<- vector()
  
  for(i in 1:length(V_tsr)){
    
    Cp_2[i]<- Cp_1[which.min(abs(TSR_1-V_tsr[i]))]
    TSR_2[i]<-TSR_1[which.min(abs(TSR_1-V_tsr[i]))]
    
  }
  
  lambda_Cp[[j]]<- cbind(Cp_2,TSR_2)
  
}


colores<- c("orange","red","blue","dodgerblue4","purple","black")
pch_dif<-c(0:5)
dev.off()
for(i in 1:length(lambda_Cp)){
  #en caso de que sea mejor añadir el origen
  #x<- c(0,lambda_Cp[[i]][,2])
  #y<- c(0,lambda_Cp[[i]][,1])
  x<- lambda_Cp[[i]][,2]
  y<- lambda_Cp[[i]][,1]
  fit5<-lm(y~poly(x,5,raw=TRUE))
  xx <- seq(min(x),max(x)+0.2, by=0.01)
  
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

axis(2, at=seq(0,0.08, by=0.01),las=2)


vvientos<-vector()
for(i in 1:length(experimento_piloto)){
  
  vvientos[i]<-experimento_piloto[[i]]$`m/s`[1]
}

leyenda_veintos<-paste0(round(vvientos,digits = 2), " m/s")

legend("topright", inset=c(0,0),leyenda_veintos,pch = pch_dif, text.col = colores,ncol = 1,cex = 0.6)
