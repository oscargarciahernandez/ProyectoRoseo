#archivo binario .nc. Me hace falta RNetCDF
library(RNetCDF)
library(dplyr)
library(here)
library(openair)
library(ggplot2)
#Creo un objeto string con el nombre completo del archivo
#PATH+nombre
inputpath<- here()

inputfile<-paste(inputpath,"/bilbo1979_2017.nc",sep="" )

#Acceder al contenido del archivo
# 2 pasos: 1.Abro el archivo; 2. Leo el archivo
SATALT_ini<-open.nc(inputfile)

#Vemos un resumen del contenido
print.nc(SATALT_ini)

#2.1 Leo el tiempo
time_1<-var.get.nc(SATALT_ini,"time")

#2.2 Latitud
latitude_1<-var.get.nc(SATALT_ini,"latitude")

#2.3 Longitud
longitude_1<-var.get.nc(SATALT_ini,"longitude")

#seleccionamos el lugar introduciendo las cordenadas en lonref y latref

lonref0<- 357.054; latref0<- 43.258

idlon<-which.min(abs(longitude_1 - lonref0))
longitude_1[idlon]

idlat<-which.min(abs(latitude_1-latref0))
latitude_1[idlat]

#los datos estan organizados como (longitud,latitud,tiempo), ahora estamos obteniendo todos los datos de velocidad para un punto especifico y todo el tiempo
u10<-var.get.nc(SATALT_ini,"u10", unpack = T)
#Corregir por el factor de escala
#u10<-u10* 0.0005974113 + 6.843877 #multiplicado por factor de escala + offset u10


#2.4 10 metre  Vwind component
v10<-var.get.nc(SATALT_ini,"v10",unpack = T)
#Corregir por el factor de escala
#v10<-v10*0.0004953484-0.2738096 ##multiplicado por factor de escala + offset v10

wind_abs = sqrt(u10^2 + v10^2)
wind_dir_trig_to = atan2(u10/wind_abs, v10/wind_abs) 
wind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi ## -111.6 degrees
ind_dir_trig_from_degrees = wind_dir_trig_to_degrees + 180 ## 68.38 degrees
summary(wind_abs)


#velocidad a 80 metros
wind_abs80<-wind_abs*log(80/1) / log(10/1)  #rugosidad urbana 
summary(wind_abs80)


#tablita(latitude_1,longitude_1,time_2,u10,v10)
#tabla<-cbind(longitude_1,latitude_1,time_1,wind_abs,ind_dir_trig_from_degrees)
#tabla<- as.data.frame(tabla)

tabla_loc<-expand.grid(longitude_1,latitude_1)
tabla_loc_time<- expand.grid(tabla_loc[,1],time_1)
tabla<- as.data.frame(cbind(tabla_loc_time[,1],tabla_loc[,2],tabla_loc_time[,2],
                            wind_abs,ind_dir_trig_from_degrees))

names(tabla)<- c("longitud","latitud","time","ws","wd")

lon<- unique(tabla$longitud)
lat<-unique(tabla$latitud)



tabla_localizacion<-tabla[tabla$longitud==lon[2] & tabla$latitud==lat[2],] 
tabla_localizacion<-tabla_localizacion %>% mutate(grup_vel=cut(tabla_localizacion$ws,
                                           seq(0,max(tabla_localizacion$ws),by=0.5),
                                           labels = seq(0.5,max(tabla_localizacion$ws),by=0.5), 
                                           include.lowest = T,right = T))

tabla_localizacion_altura<- as.data.frame(tabla_localizacion %>% mutate(ws=ws*log(50/1) / log(10/1))) 
tabla_localizacion_altura<-tabla_localizacion_altura %>% mutate(grup_vel=cut(tabla_localizacion_altura$ws,
                       seq(0,max(tabla_localizacion_altura$ws),by=0.5),
                       labels = seq(0.5,max(tabla_localizacion_altura$ws),by=0.5), 
                       include.lowest = T,right = T))



### representamos windrose a la altura del edifcio
path_here<-paste0(here(),"/graficas_rosas/")
breaks_rose<-length(seq(0,max(tabla_localizacion_altura$ws),by=2))


tiff(paste0(path_here,"rosa_elegida_he.tiff"), width = 7, height =7, units = 'in', res = 300)


windRose(tabla_localizacion_altura, angle = 22.5,
         breaks = breaks_rose,paddle = F, annotate = F,
         key.position = "right")
dev.off()  



## REalizamos barplot comparativo
distribuciones_velocidad<- table(tabla_localizacion_altura$grup_vel)
dist_total<-sum(distribuciones_velocidad)
distribuciones_velocidad_porcentaje<- distribuciones_velocidad/dist_total
distribuciones_velocidad_anual<- distribuciones_velocidad_porcentaje*8600



distribuciones_velocidad10<- table(tabla_localizacion$grup_vel)
dist_total10<-sum(distribuciones_velocidad10)
distribuciones_velocidad_porcentaje10<- distribuciones_velocidad10/dist_total10
distribuciones_velocidad_anual10<- distribuciones_velocidad_porcentaje10*8600


add.col<-function(df, new.col) {n.row<-dim(df)[1]
length(new.col)<-n.row
cbind(df, new.col)
}

frame_barplot<- as.data.frame(add.col(distribuciones_velocidad_anual,distribuciones_velocidad_anual10))
frame_barplot<- as.data.frame(cbind(frame_barplot$df,ifelse(is.na(frame_barplot$new.col), 0,frame_barplot$new.col)))
names(frame_barplot)<- c("Dist_50","Dist_10")
row.names(frame_barplot)<- names(distribuciones_velocidad_anual)


ggplot(frame_barplot)+
  geom_bar(aes(x=as.numeric(row.names(frame_barplot)),y=Dist_50),stat = "identity",alpha=.95,fill='lightblue',color='lightblue4', show.legend = T)+
  geom_bar(aes(x=as.numeric(row.names(frame_barplot)),y=Dist_10),stat = "identity", alpha=.3,fill='pink',color='red',show.legend = T)+
xlab("Velocidad del viento")+
ylab("Horas anuales") +
 geom_point(x=18, y =500, shape=22, size=5, alpha=.95,fill='lightblue',color='lightblue4')+
geom_point(x=18, y =400, shape=22, size=5, alpha=.3,fill='pink',color='red')+
annotate("text",label="Distribución de la velocidad del viento a 50 metros", x = 30, y = 500)+
  annotate("text",label="Distribución de la velocidad del viento a 10 metros", x = 30, y = 400)+
  theme_bw()

path_here<-paste0(here(),"/barplot/")   
dir.create(path_here)

ggplot(frame_barplot)+
  geom_bar(aes(x=as.numeric(row.names(frame_barplot)),y=Dist_50),stat = "identity",alpha=.95,fill='lightblue',color='lightblue4', show.legend = T)+
  geom_bar(aes(x=as.numeric(row.names(frame_barplot)),y=Dist_10),stat = "identity", alpha=.3,fill='pink',color='red',show.legend = T)+
  xlab("Velocidad del viento (m/s)")+
  ylab("Horas anuales") +
  geom_point(x=18, y =500, shape=22, size=5, alpha=.95,fill='lightblue',color='lightblue4')+
  geom_point(x=18, y =400, shape=22, size=5, alpha=.3,fill='pink',color='red')+
  annotate("text",label="Distribución de la velocidad del viento a 50 metros", x = 30, y = 500)+
  annotate("text",label="Distribución de la velocidad del viento a 10 metros", x = 30, y = 400)+
  theme_bw()

ggsave(paste0(path_here,"barplotcomparativo.tiff"), device = "tiff", dpi=1200,width =8, height =7, units = 'in')


 


tabla_localizacion_NO<-tabla_localizacion_altura[tabla_localizacion_altura$wd < 350 & tabla_localizacion_altura$wd > 314,]
distribuciones_velocidad_NO<- table(tabla_localizacion_NO$grup_vel)
dist_total_NO<-sum(distribuciones_velocidad_NO)
distribuciones_velocidad_porcentaje_NO<- distribuciones_velocidad_NO/dist_total
distribuciones_velocidad_anual_NO<- distribuciones_velocidad_porcentaje_NO*8600
tabla_NO<-as.data.frame( cbind(distribuciones_velocidad_anual_NO))
names(tabla_NO)<- "distribucion"

ggplot(tabla_NO)+
  geom_bar(aes(x=as.numeric(row.names(tabla_NO)),y=distribucion),
           stat = "identity",alpha=.95,fill='lightblue',color='lightblue4')+
  xlab("Velocidad del viento (m/s)")+
  ylab("Horas anuales") +
  ggtitle("Distribución de la velocidad del viento  \n dirección Noroeste")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
 
ggsave(paste0(path_here,"barplotNO.tiff"), device = "tiff", dpi=1200,width =8, height =7, units = 'in')
       







###Creamos un barplot comparativo entre todas las direcciones y la direccion noroeste

frame_barplot_NO<- as.data.frame(add.col(distribuciones_velocidad_anual,distribuciones_velocidad_anual_NO))
frame_barplot_NO<- as.data.frame(cbind(frame_barplot_NO$df,ifelse(is.na(frame_barplot_NO$new.col), 0,frame_barplot_NO$new.col)))
names(frame_barplot_NO)<- c("Dist_total","Dist_NO")
row.names(frame_barplot_NO)<- names(distribuciones_velocidad_anual)







ggplot(frame_barplot_NO)+
  geom_bar(aes(x=as.numeric(row.names(frame_barplot_NO)),y=Dist_total),stat = "identity",alpha=.95,fill='lightblue',color='lightblue4', show.legend = T)+
  geom_bar(aes(x=as.numeric(row.names(frame_barplot_NO)),y=Dist_NO),stat = "identity", alpha=.4,fill='pink',color='red',show.legend = T)+
  xlab("Velocidad del viento (m/s)")+
  ylab("Horas anuales") +
  geom_point(x=22, y =300, shape=22, size=5, alpha=.95,fill='lightblue',color='lightblue4')+
  geom_point(x=22, y =200, shape=22, size=5, alpha=.3,fill='pink',color='red')+
  annotate("text",label="Distribución de la velocidad del \n viento en todas las direcciones", x = 30, y = 300)+
  annotate("text",label="Distribución de la velocidad del \n viento en la dirección Noroeste", x = 30, y = 200)+
  ggtitle("Comparativa de las distribuciones del viento \n en todas las direcciones y en la dirección aprovechable")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


ggsave(paste0(path_here,"barplotNO_comparativa.tiff"), device = "tiff", dpi=1200,width =8, height =7, units = 'in')






#### ya nos ponemos al calculo de la energia anual producida. Empleando los coeficien
## la formula de ajuste es y ~ b + a*x^3 
V_viento<-seq(0,max(tabla_localizacion_altura$ws),by=0.5)
select_groups <- function(data, groups) {
  data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]
}
group_number<-length(attr(group_by(df,experimento,angulo), "group"))

coeficientes_Curva_P_V_medida<- grafica_Potencia_V(df,limitex,limitey,1)
coeficientes_Curva_P_V_estandar<- grafica_Potencia_V(df,limitex,limitey,2)
coeficientes_Curva_P_V_alfa<- grafica_Potencia_V_alfa(df,limitex,limitey)

lista_energias<- list()


for (i in 1:group_number) {
  coefs_medido<- coeficientes_Curva_P_V_alfa %>% group_by(., Experimento,Angulo) %>% select_groups(i)
  coefs_estandar<-  coeficientes_Curva_P_V_estandar %>% group_by(., Experimento,Angulo) %>% select_groups(i)
  
  Curva_de_potencia_medida<- coefs_medido$b+coefs_medido$a*V_viento^3
  Curva_de_potencia_medida<-replace(Curva_de_potencia_medida,which(Curva_de_potencia_medida<0),0)
  Curva_de_potencia_medida<- Curva_de_potencia_medida[2:80]
  
  Curva_de_potencia_estandar<- coefs_estandar$b+coefs_estandar$a*V_viento^3
  Curva_de_potencia_estandar<-replace(Curva_de_potencia_estandar,which(Curva_de_potencia_estandar<0),0)
  Curva_de_potencia_estandar<- Curva_de_potencia_estandar[2:80]
  
  
  tabla_dist_pot<- as.data.frame(cbind(tabla_NO, Curva_de_potencia_estandar,Curva_de_potencia_medida))
  names(tabla_dist_pot)<- c("horas", "est","med")
  energias<- tabla_dist_pot %>% mutate(energia_est= est*horas, energia_med=med*horas)
  
  lista_energias[[i]]<- energias
  
  }

names(lista_energias)<- paste0(coeficientes_Curva_P_V_medida[,1],coeficientes_Curva_P_V_medida[,2])




#barplot_energias_estandar<-function(){
  tabla_energia_est<-as.data.frame(matrix(unlist(lapply(lista_energias,"[",,4), use.names = F), byrow = F,ncol = 5))
  names(tabla_energia_est)<- names(lista_energias)
  row.names(tabla_energia_est)<- names(distribuciones_velocidad_anual)
  
  path_here<-paste0(here(),"/barplot/")
  
  
  
  
  ggplot(tabla_energia_est)+
    geom_bar(aes(x=as.numeric(row.names(tabla_energia_est)),y=concentrador30),stat = "identity",alpha=.9,fill='lightblue',color='lightblue4', show.legend = T)+
    geom_bar(aes(x=as.numeric(row.names(tabla_energia_est)),y=concentrador45),stat = "identity", alpha=.8,fill='pink',color='red',show.legend = T)+
    geom_bar(aes(x=as.numeric(row.names(tabla_energia_est)),y=concentrador70),stat = "identity",alpha=.7,fill='green2',color='green4', show.legend = T)+
    geom_bar(aes(x=as.numeric(row.names(tabla_energia_est)),y=paredNA),stat = "identity", alpha=.6,fill='mediumorchid1',color='mediumorchid4',show.legend = T)+
    geom_bar(aes(x=as.numeric(row.names(tabla_energia_est)),y=pilotoNA),stat = "identity",alpha=.5,fill='orange',color='orange4', show.legend = T)+
    xlab("Velocidad del viento (m/s)")+
    ylab("Energía (W/h)") +
    geom_point(x=25, y =200, shape=22, size=5, alpha=.5,fill='lightblue',color='lightblue4')+
    geom_point(x=25, y =180, shape=22, size=5, alpha=.4,fill='pink',color='red')+
    geom_point(x=25, y =160, shape=22, size=5, alpha=.3,fill='green2',color='green4')+
    geom_point(x=25, y =140, shape=22, size=5, alpha=.2,fill='mediumorchid1',color='mediumorchid4')+ 
    geom_point(x=25, y =120, shape=22, size=5,alpha=.1,fill='orange',color='orange4')+
    annotate("text",label="Concentrador 30º", x = 30, y = 200)+
    annotate("text",label="Concentrador 45º", x = 30, y = 180)+
    annotate("text",label="Concentrador 70º", x = 30, y = 160)+
    annotate("text",label="Pared", x = 27.5, y = 140)+
    annotate("text",label="Piloto", x = 27.5, y = 120)+
    ggtitle("Comparativa de las energías anuales producidas empleando las 5 configuraciones \n usando las curvas de potencia generadas con la velocidad estándar")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0(path_here,"barplotenergíacomparativa_estandar.tiff"), device = "tiff", dpi=1200,width =8, height =7, units = 'in')
  
  return(as.data.frame(colSums(tabla_energia_est)))
  
  
  
}

barplot_energias_medida<-function(){
  tabla_energia_est<-as.data.frame(matrix(unlist(lapply(lista_energias,"[",,5), use.names = F), byrow = F,ncol = 5))
  names(tabla_energia_est)<- names(lista_energias)
  row.names(tabla_energia_est)<- names(distribuciones_velocidad_anual)
  
  path_here<-paste0(here(),"/barplot/")
  
  
  
  
  ggplot(tabla_energia_est)+
    geom_bar(aes(x=as.numeric(row.names(tabla_energia_est)),y=concentrador45),stat = "identity", alpha=.8,fill='pink',color='red',show.legend = T)+
    geom_bar(aes(x=as.numeric(row.names(tabla_energia_est)),y=concentrador30),stat = "identity",alpha=.9,fill='lightblue',color='lightblue4', show.legend = T)+
    geom_bar(aes(x=as.numeric(row.names(tabla_energia_est)),y=concentrador70),stat = "identity",alpha=.7,fill='green2',color='green4', show.legend = T)+
    geom_bar(aes(x=as.numeric(row.names(tabla_energia_est)),y=paredNA),stat = "identity", alpha=.6,fill='mediumorchid1',color='mediumorchid4',show.legend = T)+
    geom_bar(aes(x=as.numeric(row.names(tabla_energia_est)),y=pilotoNA),stat = "identity",alpha=.5,fill='orange',color='orange4', show.legend = T)+
    xlab("Velocidad del viento (m/s)")+
    ylab("Energía (W/h)") +    
    geom_point(x=30, y =1000, shape=22, size=5, alpha=.1,fill='pink',color='red')+
    geom_point(x=30, y =850, shape=22, size=5, alpha=.9,fill='lightblue',color='lightblue4')+
    geom_point(x=30, y =700, shape=22, size=5, alpha=.3,fill='green2',color='green4')+
    geom_point(x=30, y =550, shape=22, size=5, alpha=.2,fill='mediumorchid1',color='mediumorchid4')+ 
    geom_point(x=30, y =400, shape=22, size=5,alpha=.1,fill='orange',color='orange4')+
    annotate("text",label="Concentrador 30º", x = 35, y = 850)+
    annotate("text",label="Concentrador 45º", x = 35, y = 1000)+
    annotate("text",label="Concentrador 70º", x = 35, y = 700)+
    annotate("text",label="Pared", x = 32.5, y = 550)+
    annotate("text",label="Piloto", x = 32.5, y = 400)+
    ggtitle("Comparativa de las energías anuales producidas empleando las 5 configuraciones")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0(path_here,"barplotenergíacomparativa_medida.tiff"), device = "tiff", dpi=1200,width =8, height =7, units = 'in')
  
  return(as.data.frame(colSums(tabla_energia_est)))
  
  
}



#energia_anual_est<-barplot_energias_estandar()
energia_anual_med<-barplot_energias_medida()


