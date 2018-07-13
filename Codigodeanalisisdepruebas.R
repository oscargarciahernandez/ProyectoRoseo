setwd("F:/pruebaslaboratorio")
inputpaths1<-"F:/pruebaslaboratorio/"
inputpaths2<- c("prueba_50/","prueba_60/","prueba_70/","prueba_80/","prueba_90/","prueba_100/")
inputpath<-paste0(inputpaths1,inputpaths2)
resistencia_50<-c(40,80,110,150,200,400,
                  700,1000,1500,2000,5000,8000)

resistencia_60<-c(10,15,20,40,60,70,80,
                  90,100,110,120,130,140,
                  150,160,170,180,190,200,
                  210,220,230,240,250,260,
                  270,280,290,300,310,320,
                  330,340,350,360,370,380,
                  390,400,420,440,460,480,
                  500,540,580,640,700,820,
                  940,1100,2500,5000,7000,8000)

resistencia_70<-c(10,20,40,80,120,130,140,
                  150,160,170,180,190,200,
                  210,220,240,260,280,300,
                  320,350,400,450,600,800,
                  1000,2000,4000,7000,8000)

resistencia_80<-c(5,10,20,30,40,50,60,70,80,
                  90,100,120,140,160,180,200,
                  220,240,260,300,350,500,700,
                  1000,2000,4000,7000,8000)

resistencia_90<-c("1_5",3,6,12,20,40,70,100,120,
                  140,150,160,180,200,250,300,
                  400,500,800,1500,5000,7000)

resistencia_100<- c(1,2,4,8,15,30,50,70,90,100,
                    120,140,150,170,200,250,400,
                    700,1500,3000,7000,8000)

resistencias<-list(resistencia_50,resistencia_60,resistencia_70,
                   resistencia_80,resistencia_90,
                   resistencia_100)


experimento_piloto<-list()
for(j in 1:length(resistencias)){
  
  setwd(inputpath[j])
  
  res_txt<-paste0(resistencias[[j]], ".txt")
  res_csv<-paste0(resistencias[[j]], ".csv")
  experimento<- list()
  for(i in 1:length(resistencias[[j]])){
    x<-colMeans(as.data.frame(read.table(res_txt[i])))
    v<-as.data.frame(as.numeric(as.character(read.table("Vviento.txt")[,1])))
    v<- v[complete.cases(v[,1]),1]
    v<-mean(v)
    y<-read.table(res_csv[1], sep ="," )
    y<-read.table(res_csv[i], sep ="," )
    yy<- colMeans(as.data.frame(cbind(as.numeric(paste0(y[,1],".",y[,2])),
                                      as.numeric(y[,3]+y[,4]*10^(-4)))))
    z<-as.data.frame( cbind(x[1],v,yy[1],yy[2]))
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
  
  plot(NULL,xlim=c(0,1.2),
       ylim = c(0,0.07),cex=0.005, yaxt ="n",
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



