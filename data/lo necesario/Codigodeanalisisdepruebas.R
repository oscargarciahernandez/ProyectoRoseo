setwd("C:/TFG/pruebas laboratorio/prueba 60_27.2C/")
resistencia<- c(7000,6000,5000,4000,3000,2000,1000,500,250,125,60,30)
res_txt<-paste0(resistencia, ".txt")
res_csv<-paste0(resistencia, ".csv")
experimento_80<- list()
for(i in 1:length(resistencia)){
  x<-read.table(res_txt[i])
  y<-read.table(res_csv[i], sep ="," )
  yy<- as.data.frame(cbind(as.numeric(paste0(y[,1],".",y[,2])),
                           as.numeric(y[,3]+y[,4]*10^(-4))))
  z<-as.data.frame(cbind(x,yy))
  names(z)<- c("RPM","m/s","V", "A")
  experimento_80[[i]]<- z
}


medias_points<-sapply(experimento_80, colMeans)
medias_points<-as.data.frame(medias_points)
names(medias_points)<- as.character(resistencia)
medias_points<-as.data.frame(t(medias_points))


watts<-vector()
for (i in 1:length(medias_points[,1])) {
  watts[i]<- medias_points[i,3]*medias_points[i,4]
}
watts<-as.data.frame(t(watts))

tabla_definitiva<-cbind(medias_points$RPM,medias_points$`m/s`,watts)




###TSR
##calculamos rad/s
r<-0.27/2

TSR<-as.data.frame(t(((medias_points$RPM*2*pi/60)*r)/medias_points$`m/s`))
pviento<-0.5*1.2*0.27*0.45*(medias_points$`m/s`)^3
cp<- t(as.vector(watts/pviento))

plot(1:length(cp),rev(cp),xaxt="n",xlab ="TSR", ylab = "CP", type = "l")
axis(1, at=1:length(cp), round(rev(TSR),digits = 2),las=2)


##buscar entre que valores esta la resistencia para Cpmax
resistencia[which.max(cp)-1]
resistencia[which.max(cp)]
resistencia[which.max(cp)+1]


