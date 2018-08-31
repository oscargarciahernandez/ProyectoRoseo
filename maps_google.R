install.packages("ggmap")
library(ggmap)



prueba_map<-get_googlemap(center = c((sum(lon)/length(lon)-360),
                                 (sum(lat)/length(lat))),zoom = 10, 
                    maptype = c("terrain"),style=c(feature='feature:road|element:all|visibility:off&style=feature:administrative.locality|element:labels|visibility:off'))

zoom<-attr(prueba_map, "zoom")

###Puntos ERa-Interim
puntos<-expand.grid(lon-360,lat)
names(puntos)<- c("lon", "lat")

##coordenadas edificio
puntos_edificio<-as.data.frame(cbind(357.054-360, 43.258))

names(puntos_edificio)<- c("lon", "lat")

path_here<-paste0(here(),"/mapas/")
dir.create(path_here)
tiff(paste0(path_here,"mapzoom",zoom,".tiff"), width = 7, height =7, units = 'in', res = 300)

ggmap(prueba_map)+
  geom_point(data = puntos, aes(lon,lat), size=3)+
  geom_point(data = puntos_edificio, aes(lon,lat),size=3, colour="red")

dev.off()  



prueba_map_con_etiquetas<-get_googlemap(center = c((sum(lon)/length(lon)-360),
                                     (sum(lat)/length(lat))),zoom = 10, 
                          maptype = c("terrain"),style=c(feature='feature:road|element:all|visibility:off&style=feature:administrative.locality|element:labels|visibility:simplified'))

  

puntos<-expand.grid(lon-360,lat)
names(puntos)<- c("lon", "lat")


path_here<-paste0(here(),"/mapas/")
dir.create(path_here)
tiff(paste0(path_here,"maplabels_zoom",zoom,".tiff"), width = 7, height =7, units = 'in', res = 300)

ggmap(prueba_map_con_etiquetas)+
  geom_point(data = puntos, aes(lon,lat), size=3, colour = "black", alpha=0.7)+
  geom_point(data = puntos_edificio, aes(lon,lat),size=3, colour="red",alpha=0.7)

dev.off()  

tabla_prueba<- tabla

plot.windrose(tabla, spd = "ws",dir = "wd")
  
oyee<- tabla_prueba%>%group_by(., longitud,latitud)%>% do(subplots= plot.windrose(., spd = "ws",dir="wd",dirres = 22.5,spdres = 1,spdseq = seq(0,20,1)))%>%
mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                         x = lon - 1,      # change from 1 to other 
                                         y = lat - 1,      # values if necessary,
                                         xmax = lon + 1,   # depending on the map's
                                         ymax = lat + 1))) # resolution.

oyee_1<- ggmap(prueba_map_con_etiquetas)+coord_fixed()

oyee_1+oyee$subgrobs
