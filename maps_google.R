<<<<<<< HEAD


funcion_mapas<- function(opacidad, zoom_in){
  library(ggmap)
  library(here)
  ## ejecutamos los scripts de extracción de datos bilbao.nc y de la windrose
  ## ejecutamos solo "vuelta.R si aun no se ha ejectuado
  if(exists("tabla")){
    tabla_prueba<- tabla
    
    
  }else{
    source(here('vuelta.R'))
    tabla_prueba<- tabla}
  
  
  source(here('Windrose_sin_nada.R'))
  
  
  
  
  
  
  
  ###Puntos ERa-Interim
  puntos<-expand.grid(lon-360,lat)
  names(puntos)<- c("lon", "lat")
  
  ##coordenadas edificio
  puntos_edificio<-as.data.frame(cbind(357.054-360, 43.258))
  
  names(puntos_edificio)<- c("lon", "lat")
  
  
  
  #tiff(paste0(path_here,"mapzoom",zoom,".tiff"), width = 7, height =7, units = 'in', res = 300)
  
  #ggmap(prueba_map)+
  #geom_point(data = puntos, aes(lon,lat), size=3)+
  #geom_point(data = puntos_edificio, aes(lon,lat),size=3, colour="red")
  
  #dev.off()  
  
  
  
  prueba_map_con_etiquetas<-get_googlemap(center = c(357.054-360, 43.258),zoom = zoom_in, 
                                          maptype = c("terrain"),style=c(feature='feature:road|element:all|visibility:off&style=feature:administrative.locality|element:labels|visibility:simplified'))
  
  prueba_map_sin_etiquetas<-get_googlemap(center = c(357.054-360, 43.258),zoom = zoom_in,source= "cloudmap",maptype = c("terrain"),style=c(feature='feature:road|element:all|visibility:off&style=feature:administrative|element:labels|visibility:off&style=feature:poi|element:labels|visibility:off&style=feature:transit|element:labels|visibility:off'))
  
  
  
  path_here<-paste0(here(),"/mapas/")
  dir.create(path_here)
  
  ggmap(prueba_map_con_etiquetas)
  ggsave(paste0(path_here,"mapasinpuntosetiquetas_zoom",zoom_in,".tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')
  
  
  
  
  
  ggmap(prueba_map_sin_etiquetas)+
    geom_point(data = puntos, aes(lon,lat), size=3, colour = "black", alpha=0.7)+
    geom_point(data = puntos_edificio, aes(lon,lat),size=3, colour="red",alpha=0.7)
  
  ggsave(paste0(path_here,"mapaconpuntos_zoom",zoom_in,".tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')
  
  
  
  
  
  
  
  #### Añadimos rosas
  
  ##creamos un mapa semitransparente
  coef_opacidad<- opacidad
  opacidad <- attributes(prueba_map_sin_etiquetas)
  opacidad2 <- matrix(adjustcolor(prueba_map_sin_etiquetas, 
                                  alpha.f = coef_opacidad), 
                      nrow = nrow(prueba_map_sin_etiquetas))
  attributes(opacidad2) <- opacidad
  ploteo_mapa<- ggmap(opacidad2)+coord_fixed()
  ploteo_rosas<- tabla_prueba%>%group_by(., longitud,latitud)%>% do(subplots= plot.windrose(., spd = "ws",dir="wd",dirres = 22.5,spdres = 2,spdseq = c(0,1,2,3,5,7,10,15), palette = "PuRd"))%>%
    mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                             x = longitud-360-0.1,      # change from 1 to other 
                                             y = latitud-0.1,      # values if necessary,
                                             xmax = longitud-360+0.1,   # depending on the map's
                                             ymax = latitud+0.1))) # resolution.
  
  
  
  
  ploteo_mapa+ploteo_rosas$subgrobs+
    geom_point(data = puntos_edificio, aes(lon,lat),size=2,shape=4, colour="black",alpha=0.7)
  
  
  ggsave(paste0(path_here,"mapa_rosasopacidad",coef_opacidad,"zoom",zoom_in,".tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')
  
  
  
  
}
ploteo_rosas_individual<- function(){
  
  library(here)
  source(here('vuelta.R'))
  
  
  ###ploteo de rosas de los vientos
  lon<- unique(tabla$longitud)
  lat<-unique(tabla$latitud)
  for (longitud in 1:length(lon)) {
    
    tabla_lon<- tabla[tabla$longitud == lon[longitud],]
    
    for(latitude in 1:length(lat)){
      
      tabla_lon_lat<- tabla_lon[tabla_lon$latitud==lat[latitude],]
      
      if(length(tabla_lon_lat[,1])==0){
        
      }else{
        prueba<-as.data.frame(cbind(tabla_lon_lat$ws,tabla_lon_lat$wd))
        colnames(prueba)<- c("ws","wd")
        
        breaks_rose<-length(seq(0,max(prueba[,1]),by=1))
        
        path_here<-paste0(here(),"/graficas_rosas/")
        dir.create(path_here)
        
        
        
        tiff(paste0(path_here,lat[latitude],"_",lon[longitud],".tiff"), width = 7, height =7, units = 'in', res = 300)
        
        windRose(prueba,ws.int = 1,angle = 22.5,breaks = breaks_rose,
                 paddle = F, annotate = F,key.position = "right")
        #par(new=T)
        #subtitle<- paste0("Lon = ", lon[longitud], "Lat = ", lat[latitude])
        #titulo<- paste0("Rosa de los vientos \n", subtitle)
        #title(main = titulo)
        dev.off()  
      }
      
      
      
    }
    
  }
}
=======


funcion_mapas<- function(opacidad, zoom_in){
  library(ggmap)
  library(here)
  ## ejecutamos los scripts de extracción de datos bilbao.nc y de la windrose
  ## ejecutamos solo "vuelta.R si aun no se ha ejectuado
  if(exists("tabla")){
    tabla_prueba<- tabla
    
    
  }else{
    source(here('vuelta.R'))
    tabla_prueba<- tabla}
  
  
  source(here('Windrose_sin_nada.R'))
  
  
  
  
  
  
  
  ###Puntos ERa-Interim
  puntos<-expand.grid(lon-360,lat)
  names(puntos)<- c("lon", "lat")
  
  ##coordenadas edificio
  puntos_edificio<-as.data.frame(cbind(357.054-360, 43.258))
  
  names(puntos_edificio)<- c("lon", "lat")
  
  
  
  #tiff(paste0(path_here,"mapzoom",zoom,".tiff"), width = 7, height =7, units = 'in', res = 300)
  
  #ggmap(prueba_map)+
  #geom_point(data = puntos, aes(lon,lat), size=3)+
  #geom_point(data = puntos_edificio, aes(lon,lat),size=3, colour="red")
  
  #dev.off()  
  
  
  
  prueba_map_con_etiquetas<-get_googlemap(center = c(357.054-360, 43.258),zoom = zoom_in, 
                                          maptype = c("terrain"),style=c(feature='feature:road|element:all|visibility:off&style=feature:administrative.locality|element:labels|visibility:simplified'))
  
  prueba_map_sin_etiquetas<-get_googlemap(center = c(357.054-360, 43.258),zoom = zoom_in,source= "cloudmap",maptype = c("terrain"),style=c(feature='feature:road|element:all|visibility:off&style=feature:administrative|element:labels|visibility:off&style=feature:poi|element:labels|visibility:off&style=feature:transit|element:labels|visibility:off'))
  
  
  
  path_here<-paste0(here(),"/mapas/")
  dir.create(path_here)
  
  ggmap(prueba_map_con_etiquetas)
  ggsave(paste0(path_here,"mapasinpuntosetiquetas_zoom",zoom_in,".tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')
  
  
  
  
  
  ggmap(prueba_map_sin_etiquetas)+
    geom_point(data = puntos, aes(lon,lat), size=3, colour = "black", alpha=0.7)+
    geom_point(data = puntos_edificio, aes(lon,lat),size=3, colour="red",alpha=0.7)
  
  ggsave(paste0(path_here,"mapaconpuntos_zoom",zoom_in,".tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')
  
  
  
  
  
  
  
  #### Añadimos rosas
  
  ##creamos un mapa semitransparente
  coef_opacidad<- opacidad
  opacidad <- attributes(prueba_map_sin_etiquetas)
  opacidad2 <- matrix(adjustcolor(prueba_map_sin_etiquetas, 
                                  alpha.f = coef_opacidad), 
                      nrow = nrow(prueba_map_sin_etiquetas))
  attributes(opacidad2) <- opacidad
  ploteo_mapa<- ggmap(opacidad2)+coord_fixed()
  ploteo_rosas<- tabla_prueba%>%group_by(., longitud,latitud)%>% do(subplots= plot.windrose(., spd = "ws",dir="wd",dirres = 22.5,spdres = 2,spdseq = c(0,1,2,3,5,7,10,15), palette = "PuRd"))%>%
    mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                             x = longitud-360-0.1,      # change from 1 to other 
                                             y = latitud-0.1,      # values if necessary,
                                             xmax = longitud-360+0.1,   # depending on the map's
                                             ymax = latitud+0.1))) # resolution.
  
  
  
  
  ploteo_mapa+ploteo_rosas$subgrobs+
    geom_point(data = puntos_edificio, aes(lon,lat),size=2,shape=4, colour="black",alpha=0.7)
  
  
  ggsave(paste0(path_here,"mapa_rosasopacidad",coef_opacidad,"zoom",zoom_in,".tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')
  
  
  
  
}
ploteo_rosas_individual<- function(){
  
  library(here)
  source(here('vuelta.R'))
  
  
  ###ploteo de rosas de los vientos
  lon<- unique(tabla$longitud)
  lat<-unique(tabla$latitud)
  for (longitud in 1:length(lon)) {
    
    tabla_lon<- tabla[tabla$longitud == lon[longitud],]
    
    for(latitude in 1:length(lat)){
      
      tabla_lon_lat<- tabla_lon[tabla_lon$latitud==lat[latitude],]
      
      if(length(tabla_lon_lat[,1])==0){
        
      }else{
        prueba<-as.data.frame(cbind(tabla_lon_lat$ws,tabla_lon_lat$wd))
        colnames(prueba)<- c("ws","wd")
        
        breaks_rose<-length(seq(0,max(prueba[,1]),by=1))
        
        path_here<-paste0(here(),"/graficas_rosas/")
        dir.create(path_here)
        
        
        
        tiff(paste0(path_here,lat[latitude],"_",lon[longitud],".tiff"), width = 7, height =7, units = 'in', res = 300)
        
        windRose(prueba,ws.int = 1,angle = 22.5,breaks = breaks_rose,
                 paddle = F, annotate = F,key.position = "right")
        #par(new=T)
        #subtitle<- paste0("Lon = ", lon[longitud], "Lat = ", lat[latitude])
        #titulo<- paste0("Rosa de los vientos \n", subtitle)
        #title(main = titulo)
        dev.off()  
      }
      
      
      
    }
    
  }
}
>>>>>>> c68470beb807859eef3e5b9bfff3b7f944981179
