library(tidyverse)
library(httr)
library(XML)
library(stringr)
library(openair)
library(chron)
library(lubridate)


id_anem_U<-"0B38DAE79059"
id_anem_H<-"0B75FE3A4FB6"
phone_id<-"&phoneid=640689911849"
enla1<-"https://measurements.mobile-alerts.eu/Home/MeasurementDetails?"







######### Anemómetro de la UNIVERSIDAD
## hacemos una primera peticion de datos para ver
## hasta que página existen datos. 

U_link<- "https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid=0B38DAE79059&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
U_req<- POST(U_link, encode = "form")
U_req2<-htmlParse(U_req, asText = TRUE)
U_req3<-xpathApply(U_req2, "//a[@href]", xmlGetAttr, "href")
pos<- length(U_req3)
node<-as.character(U_req3[pos])

##número de páginas
if(is.na(str_extract(node,"(?<=page=)\\d\\d\\d"))){
  p<- as.numeric(str_extract(node,"(?<=page=)\\d\\d"))  
}else{
  p<- as.numeric(str_extract(node,"(?<=page=)\\d\\d\\d"))  
  
}

#######Valores por página
vpp<-length(str_extract_all(t(xmlToDataFrame(getNodeSet(U_req2,
                                                                 "//tbody"))),
                                     boundary("word")))

### (1526901420)-21/5/2018 13:17:00  
### inicio de recopilación de datos

U_lnk<-"deviceid=0B38DAE79059&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"

U_dt<- matrix(-31, ncol = 10, nrow = vpp*p)
k<-1
for (j in 1:p) {
  enlace<- paste0(enla1,"page=",j,"&",U_lnk)
  request2<- POST(enlace, encode = "form")
  request_list2<- htmlParse(request2, asText = TRUE)
  rr<-getNodeSet(request_list2, "//tbody")
  rr1<- xmlToDataFrame(rr)
  rr1<-t(rr1)
  rr2<-str_extract_all(rr1, boundary("word"))
  
  
  for (i in 1:length(rr2)) {
    month<- as.numeric(rr2[[i]][1])
    day<- as.numeric(rr2[[i]][2])
    year<- as.numeric(rr2[[i]][3])
    hora<- as.numeric(rr2[[i]][4])
    min<- as.numeric(rr2[[i]][5])
    sec<- as.numeric(rr2[[i]][6])
    merid<- as.character(rr2[[i]][7])
    wind<-as.numeric(rr2[[i]][8])
    gust<-as.numeric(rr2[[i]][11])
    dir<-paste(rr2[[i]][14],rr2[[i]][15], sep = "-")
    
    xx<- cbind(month, day, year, hora, min, sec, merid, wind, gust, dir)
    U_dt[k,]<- xx
    k<- k+1
  }
}


colnames(U_dt)<- c("Mes", "Dia", "anno", "Hora", "Min", "Sec","Merid", "Wd mean", "wd gust", "Dir")
U_dt<- as.data.frame(U_dt)



date<- matrix(-31, nrow  = length(U_dt[,1]))
namesss<- matrix(-31, nrow = length(U_dt[,1]))
for (i in 1: length(U_dt[,1])) {
  
 fecha1<- paste0( U_dt$Dia[i],'/',
                 U_dt$Mes[i],'/',
                 U_dt$anno[i])
 
 fecha2<-paste0(U_dt$Hora[i], ':',
                U_dt$Min[i],':',
                U_dt$Sec[i], ' ', U_dt$Merid[i] )
 
  fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
format =  "%H:%M:%S")      
  
  fecha<- paste0(fecha1,' ',fecha2)
 date[i]<-as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S")
 namesss[i]<-as.character(fecha)
}


date<- as.data.frame(date)
namesss<- as.data.frame(namesss)

U_dt<- cbind(date, namesss, U_dt$`Wd mean`, U_dt$`wd gust`, U_dt$Dir)
colnames(U_dt)<- c("s/since_1/1/1970", "Date_hour","Mean","Max","Dir_ch")

U_dt$Dir_ch<-str_remove(U_dt$Dir_ch, pattern = '-NA')
dir_degrees_U<-matrix(-31, nrow = length(U_dt$Dir))

for (i in 1:length(U_dt$Dir)) {
  yy<-U_dt$Dir[i]
  dir_degrees_U[i]<- ifelse(yy=="North", 0, ifelse(yy=="North-northeast",20,
                                                 ifelse(yy=="Northeast",45, ifelse(yy=="East-northeast",65,
                                                                                   ifelse(yy=="East", 90, 
                                                                                          ifelse(yy=="East-southeast", 110,
                                                                                                 ifelse(yy=="Southeast", 135,
                                                                                                        ifelse(yy=="South-Southeast", 155, 
                                                                                                               ifelse(yy=="South", 180, 
                                                                                                                      ifelse(yy=="South-southwest", 200, 
                                                                                                                             ifelse(yy=="Southwest", 225,
                                                                                                                                    ifelse(yy=="West-southwest", 245, 
                                                                                                                                           ifelse(yy=="West", 270, 
                                                                                                                                                  ifelse(yy=="West-northwest",290,
                                                                                                                                                         ifelse(yy=="Northwest", 315,
                                                                                                                                                                ifelse(yy=="North-northwest", 335, 
                                                                                                                                                                       ifelse(yy=='-31',NA)))))))))))))))))
}
dir_degrees_U<- as.data.frame( dir_degrees_U)
U_dt<- cbind(U_dt, dir_degrees_U)
colnames(U_dt)<- c("s/since_1/1/1970", "Date_hour","Mean","Max","Dir_ch","Dir_deg")





######### Anemómetro del Hágono

## hacemos una primera peticion de datos para ver
## hasta que página existen datos. 

H_link<- "https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid=0B75FE3A4FB6&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
H_req<- POST(H_link, encode = "form")
H_req2<-htmlParse(H_req, asText = TRUE)
H_req3<-xpathApply(H_req2, "//a[@href]", xmlGetAttr, "href")
pos<- length(H_req3)
node<-as.character(H_req3[pos])

##número de páginas
if(is.na(str_extract(node,"(?<=page=)\\d\\d\\d"))){
  p<- as.numeric(str_extract(node,"(?<=page=)\\d\\d"))  
}else{
  p<- as.numeric(str_extract(node,"(?<=page=)\\d\\d\\d"))  
  
}

#######Valores por página
vpp<-length(str_extract_all(t(xmlToDataFrame(getNodeSet(H_req2,
                                                                 "//tbody"))),
                                     boundary("word")))


### Vamos a suponer la misma fecha de inicio, 
### porque fue el d??a que mario corriguio la orientacioón
### del anemómetro. Podemos decidir incluso que los datos
### nacen el d??a 22 de mayo de 2018. 
### (1526901420)-21/5/2018 13:17:00  
### inicio de recopilación de datos
H_lnk<-"deviceid=0B75FE3A4FB6&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"

H_dt<- matrix(-31, ncol = 10, nrow = vpp*p)
k<-1
for (j in 1:p) {
  enlace<- paste0(enla1,"page=",j,"&",H_lnk)
  request2<- POST(enlace, encode = "form")
  request_list2<- htmlParse(request2, asText = TRUE)
  rr<-getNodeSet(request_list2, "//tbody")
  rr1<- xmlToDataFrame(rr)
  rr1<-t(rr1)
  rr2<-str_extract_all(rr1, boundary("word"))
  
  
  for (i in 1:length(rr2)) {
    month<- as.numeric(rr2[[i]][1])
    day<- as.numeric(rr2[[i]][2])
    year<- as.numeric(rr2[[i]][3])
    hora<- as.numeric(rr2[[i]][4])
    min<- as.numeric(rr2[[i]][5])
    sec<- as.numeric(rr2[[i]][6])
    merid<- as.character(rr2[[i]][7])
    wind<-as.numeric(rr2[[i]][8])
    gust<-as.numeric(rr2[[i]][11])
    dir<-paste(rr2[[i]][14],rr2[[i]][15], sep = "-")
    
    xx<- cbind(month, day, year, hora, min, sec, merid, wind, gust, dir)
    H_dt[k,]<- xx
    k<- k+1
  }
}


colnames(H_dt)<- c("Mes", "Dia", "anno", "Hora", "Min", "Sec","Merid", "Wd mean", "wd gust", "Dir")
H_dt<- as.data.frame(H_dt)


date<- matrix(-31, nrow  = length(H_dt[,1]))
namesss<- matrix(-31, nrow = length(H_dt[,1]))
for (i in 1: length(H_dt[,1])) {
  
  fecha1<- paste0( H_dt$Dia[i],'/',
                   H_dt$Mes[i],'/',
                   H_dt$anno[i])
  
  fecha2<-paste0(H_dt$Hora[i], ':',
                 H_dt$Min[i],':',
                 H_dt$Sec[i], ' ', H_dt$Merid[i] )
  
  fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                  format =  "%H:%M:%S")      
  
  fecha<- paste0(fecha1,' ',fecha2)
  date[i]<-as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S")
  namesss[i]<-as.character(fecha)
}


date<- as.data.frame(date)
namesss<- as.data.frame(namesss)

H_dt<- cbind(date, namesss, H_dt$`Wd mean`, H_dt$`wd gust`, H_dt$Dir)
colnames(H_dt)<- c("s/since_1/1/1970", "Date_hour","Mean","Max","Dir")


########DIR  eliminar NA 

H_dt$Dir<-str_remove(H_dt$Dir, pattern = '-NA')
dir_degrees<-matrix(-31, nrow = length(H_dt$Dir))

for (i in 1:length(H_dt$Dir)) {
  yy<-H_dt$Dir[i]
 dir_degrees[i]<- ifelse(yy=="North", 0, ifelse(yy=="North-northeast",20,
         ifelse(yy=="Northeast",45, ifelse(yy=="East-northeast",65,
                                          ifelse(yy=="East", 90, 
                                                 ifelse(yy=="East-southeast", 110,
                                                        ifelse(yy=="Southeast", 135,
                                                               ifelse(yy=="South-Southeast", 155, 
                                                                      ifelse(yy=="South", 180, 
                                                                             ifelse(yy=="South-southwest", 200, 
                                                                                    ifelse(yy=="Southwest", 225,
                                                                                           ifelse(yy=="West-southwest", 245, 
                                                                                                  ifelse(yy=="West", 270, 
                                                                                                         ifelse(yy=="West-northwest",290,
                                                                                                                ifelse(yy=="Northwest", 315,
                                                                                                                       ifelse(yy=="North-northwest", 335, 
                                                                                                                              ifelse(yy=='-31',NA)))))))))))))))))
}
dir_degrees<- as.data.frame( dir_degrees)
H_dt<- cbind(H_dt, dir_degrees)
colnames(H_dt)<- c("s/since_1/1/1970", "Date_hour","Mean","Max","Dir_ch","Dir_deg")


#############################datos validos
#consideramos que ambos anemómetros están perfectamente funcionando a partir 
#del d??a 22 a las 00:00:00

start_meas<-unclass(as.POSIXct("22/05/2018 00:00:00",format="%d/%m/%Y %H:%M:%S"))
start_meas<- start_meas[1]

condit_H<- H_dt[,1]>start_meas
H_dt_valid<-as.data.frame(H_dt[condit_H, ])
H_dt_valid<-H_dt_valid[complete.cases(H_dt_valid), ]


condit_U<- U_dt[,1]>start_meas
U_dt_valid<-as.data.frame(U_dt[condit_U, ])
U_dt_valid<-U_dt_valid[complete.cases(U_dt_valid), ]




####organización de datos, por d??a hora, mes, season. 
##Hexagono
H_timee<- dmy_hms(H_dt_valid$Date_hour)
myH_dt<- as.data.frame(cbind(H_timee,
                                 as.data.frame(H_dt_valid$Mean), 
                                 H_dt_valid$Dir_deg))
colnames(myH_dt)<-c("date","ws","wd")

Cut_H<- cutData(myH_dt, type = "weekday", hemisphere = "northern",
              local.tz = "Europe/Madrid",start.day = 1)



###universidad
U_Time<- dmy_hms(U_dt_valid$Date_hour)
myU_dt<- as.data.frame(cbind(U_Time,as.data.frame(U_dt_valid$Mean), 
                                 U_dt_valid$Dir_deg))
colnames(myU_dt)<-c("date","ws","wd")
Cut_U<- cutData(myU_dt, type = "hour", hemisphere = "northern",
                  local.tz = "Europe/Madrid",start.day = 1)




####### tabla con los datos horarios. 
###### hay 3600 segundos en una hora.

low_val<-unclass(as.POSIXct("22/05/2018 00:00:00",format="%d/%m/%Y %H:%M:%S"))[1]

hig_val_H<- H_dt[1,1]
Ttable_H<- seq(low_val, hig_val_H, by=3600)


hig_val_U<- U_dt[1,1]
Ttable_U<- seq(low_val, hig_val_U, by=3600)

H_hor<- matrix(-31, nrow = length(Ttable_H), ncol = 6)
U_hor<- matrix(-31, nrow = length(Ttable_U), ncol = 6)

for (i in 1:length(Ttable_H)) {
  con_H<- Ttable_H[i]<H_dt[,1]
  T_val_H<- H_dt_valid[con_H, ]
  T_val_H<-T_val_H[complete.cases(T_val_H),]
  T_val_H<- as.matrix(T_val_H[length(T_val_H[,1]),])
  H_hor[i,]<- T_val_H
  
  con_U<- Ttable_U[i]<U_dt[,1]
  T_val_U<- U_dt_valid[con_U, ]
  T_val_U<-T_val_U[complete.cases(T_val_U),]
  T_val_U<- as.matrix(T_val_U[length(T_val_U[,1]),])
  U_hor[i,]<- T_val_U
}

H_hor<- apply(H_hor, 2, rev)
U_hor<- apply(U_hor, 2, rev)  


######

Cut_H %>%  group_by(., weekday) %>% summarise(., Valor_Medio = mean(wd), Medidas = n())
  
  


##########Rosa de los vientos

breaks<- c(0,0.3,1,2,3,4,5,6,7,8,9,10,25)


windRose(myH_dt, ws = "ws", wd = "wd",
         ws.int = 2, angle = 22.5, type = "default", bias.corr = TRUE, cols
         = "default", grid.line = TRUE, width = 4 , seg = NULL, auto.text= FALSE, breaks = breaks, offset = 10, normalise = FALSE, max.freq =
           NULL, paddle = FALSE, key.header = "(m/s)", key.footer = NULL,
         key.position = "right", key = TRUE, dig.lab = 3, statistic ="prop.mean" , pollutant = NULL, annotate = TRUE, angle.scale =
           315, border = NULL)

