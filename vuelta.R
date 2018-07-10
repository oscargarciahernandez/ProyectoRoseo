#archivo binario .nc. Me hace falta RNetCDF
library(RNetCDF)
library(dplyr)

#Creo un objeto string con el nombre completo del archivo
#PATH+nombre
inputpath<-"C:/TFG/appshiny/Rprojecto/TFG_mario/"

inputfile<-paste(inputpath,"bilbo1979_2017.nc",sep="" )

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
u10<-var.get.nc(SATALT_ini,"u10")
#Corregir por el factor de escala
u10<-u10* 0.0005974113 + 6.843877 #multiplicado por factor de escala + offset u10


#2.4 10 metre  Vwind component
v10<-var.get.nc(SATALT_ini,"v10")
#Corregir por el factor de escala
v10<-v10*0.0004953484-0.2738096 ##multiplicado por factor de escala + offset v10

wind_abs = sqrt(u10^2 + v10^2)
wind_dir_trig_to = atan2(u10/wind_abs, v10/wind_abs) 
wind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi ## -111.6 degrees
ind_dir_trig_from_degrees = wind_dir_trig_to_degrees + 180 ## 68.38 degrees
summary(wind_abs)


#velocidad a 80 metros
wind_abs80<-wind_abs*log(80/1) / log(10/1)  #rugosidad urbana 
summary(wind_abs80)


#tablita(latitude_1,longitude_1,time_2,u10,v10)
tabla<-cbind(longitude_1,latitude_1,time_1,wind_abs,ind_dir_trig_from_degrees)
tabla<- as.data.frame(tabla)


## localizacion 8
tabla_loc8<-tabla[which(tabla$longitude_1==356.875 & tabla$latitude_1==43.375),]
grup_vel<-cut(tabla_loc8$wind_abs,seq(0,max(tabla_loc8$wind_abs),by=0.5),
              labels = seq(0.5,max(tabla_loc8$wind_abs),by=0.5), include.lowest = T,right = T)
tabla_loc8<- as.data.frame(cbind(tabla_loc8[,1:5],grup_vel))

##identificar numero de valores por cada velocidad del viento. 
dist_vel<-as.data.frame(table(tabla_loc8$grup_vel))
dist_total<-sum(dist_vel$Freq)
dist_vel_per<- as.data.frame(dist_vel$Freq/dist_total)
dist_vel_anual<-as.data.frame(dist_vel_per*8600)
names(dist_vel_anual)<-"Horas anuales"
barplot(dist_vel_anual$`Horas anuales`)


## identificar districucion del viento para Norte-Noroeste (285?, 15?)
tabla_NO<-tabla_loc8[tabla_loc8$ind_dir_trig_from_degrees>314 & tabla_loc8$ind_dir_trig_from_degrees<346,]


##Barplot sin concentrador
grup_vel_NO<-cut(tabla_NO$wind_abs,seq(0,max(tabla_NO$wind_abs),by=0.5),
                                   labels = seq(0.5,max(tabla_NO$wind_abs),by=0.5), 
                                   include.lowest = T,right = T)
tabla_NO<- as.data.frame(cbind(tabla_NO[,1:5],grup_vel_NO))

dist_vel_NO<-as.data.frame(table(tabla_NO$grup_vel))

dist_vel_NO_per<- as.data.frame(dist_vel_NO$Freq/dist_total)
dist_vel_NO_anual<-as.data.frame(dist_vel_NO_per*8600)
names(dist_vel_NO_anual)<-"horas anuales"
barplot(dist_vel_NO_anual$`horas anuales`)


##Barplot con concentrador

tabla_NO_x4<- tabla_NO$wind_abs*4

grup_vel_NO_x4<-cut(tabla_NO_x4,seq(0,max(tabla_NO_x4),by=0.5),
                 labels = seq(0.5,max(tabla_NO_x4),by=0.5), 
                 include.lowest = T,right = T)
tabla_NO_x4<- as.data.frame(cbind(tabla_NO_x4,grup_vel_NO_x4))

dist_vel_NO_x4<-as.data.frame(table(tabla_NO_x4$grup_vel))

dist_vel_NO_x4_per<- as.data.frame(dist_vel_NO_x4$Freq/dist_total)
dist_vel_NO_x4_anual<-as.data.frame(dist_vel_NO_x4_per*8600)
names(dist_vel_NO_x4_anual)<-"horas anuales"
barplot(dist_vel_NO_x4_anual$`horas anuales`)





### representar la curva de potencia del aerogeneraodor
Vviento<-c(3.13,10,17.88)
Pot<- c(1,200,1100)  
Data_curve<-data.frame(Pot,Vviento)
Vviento_serie<-seq(3.13,17.88,by=0.02)

#Fit the linear model
pot_fit<- nls(Pot ~ exp(a+b*Vviento),data =Data_curve,start=c(a=0,b=0))

#lm(Pot~exp(a+b*Vviento), data = Data_curve, duis
###ejecutas pot_fit y te dice que a=2.9527 y b=0.2266
a=2.9527
b=0.2266

# Check that the estimated coefficient is 25, just as we expected!

#Plot the fitted line
plot(Vviento, Pot)
lines(Vviento_serie, exp(a+b*Vviento_serie), col = "red")

##la curvva será potencia 
##0 de 0 a 3.13 m/s
## exp(2.9527+0.2266*Vviento ) de 3.13 a 17.88
tramo_pot<-seq(3,18,by=1)
Pot_tramo<- exp(2.9527+0.2266*tramo_pot )/1000
## 1100 de 17.88 hasta infinito

E_kwh_1_sin <- dist_vel_NO_anual[3:18,]*Pot_tramo
E_kwh_2_sin <- dist_vel_NO_anual[18:length(dist_vel_NO_anual),]*1.1

E_kwh_1_con <- dist_vel_NO_x4_anual[3:18,]*Pot_tramo
E_kwh_2_con <- dist_vel_NO_x4_anual[18:length(dist_vel_NO_x4_anual),]*1.1

# try the default settings
p0 <- plot.windrose(spd = tabla_loc$wind_abs80,
                    dir = tabla_loc$ind_di44r_trig_from_degrees)

p0 <- plot.windrose(spd = tabla_loc8$wind_abs,
                    dir = tabla_loc8$ind_dir_trig_from_degrees)
