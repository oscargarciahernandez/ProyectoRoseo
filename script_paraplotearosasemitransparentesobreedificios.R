<<<<<<< HEAD


edificio_detalle<-get_googlemap(center = c(-2.945630,43.257651),zoom = zoom_in,source= "cloudmap",maptype = c("satellite"),style=c(feature='feature:road|element:all|visibility:off&style=feature:administrative|element:labels|visibility:off&style=feature:poi|element:labels|visibility:off&style=feature:transit|element:labels|visibility:off'))


edificio_windrose<-tabla[tabla$longitud==lon[2] & tabla$latitud==lat[2], ]%>% mutate(.,longitud=-2.945630-0.00007,latitud=43.257651-0.00005)



ploteo_mapa<- ggmap(edificio_detalle)+coord_fixed()

ploteo_rosa<- edificio_windrose%>%group_by(., longitud,latitud)%>% do(subplots= plot.windrose(., spd = "ws",dir="wd",dirres = 22.5,spdres = 2,spdseq = c(0,1,2,3,5,7,10,15,20), palette = "PuRd"))%>%
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = longitud-0.001,      # change from 1 to other 
                                           y = latitud-0.001,      # values if necessary,
                                           xmax = longitud+0.001,   # depending on the map's
                                           ymax = latitud+0.001))) # resolution.

ploteo_mapa
ggsave(paste0(path_here,"mapa_detalle.tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')


ploteo_mapa+ ploteo_rosa$subgrobs
ggsave(paste0(path_here,"mapa_detallerosa.tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')






plot.windrose(prueba_windrose, spd = "ws",dir="wd",dirres = 22.5,spdres = 2, palette = "PuRd")
ggsave(paste0(path_here,"rosa_conleyenda.tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')




####### ploteo con la rosa elegida

edificio_detalle_lejano<-get_googlemap(center = c(-3.1,43.35),zoom = 11,source= "cloudmap",maptype = c("satellite"),style=c(feature='feature:road|element:all|visibility:off&style=feature:administrative|element:labels|visibility:off&style=feature:poi|element:labels|visibility:off&style=feature:transit|element:labels|visibility:off'))


edificio_windrose_lejano<-tabla[tabla$longitud==lon[2] & tabla$latitud==lat[2], ]


ploteo_mapa<- ggmap(edificio_detalle_lejano)+coord_fixed()

ploteo_rosa<- edificio_windrose_lejano%>%group_by(., longitud,latitud)%>% do(subplots= plot.windrose(., spd = "ws",dir="wd",dirres = 22.5,spdres = 2,spdseq = c(0,1,2,3,5,7,10,15,20), palette = "PuRd"))%>%
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = longitud-360-0.15,      # change from 1 to other 
                                           y = latitud-0.15,      # values if necessary,
                                           xmax = longitud-360+0.15,   # depending on the map's
                                           ymax = latitud+0.15))) # resolution.




ploteo_mapa+ ploteo_rosa$subgrobs+
  geom_point(data = puntos_edificio, aes(lon,lat),size=3, colour="red",alpha=0.7)

ggsave(paste0(path_here,"mapa_detallerosa_lejano.tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')







=======


edificio_detalle<-get_googlemap(center = c(-2.945630,43.257651),zoom = zoom_in,source= "cloudmap",maptype = c("satellite"),style=c(feature='feature:road|element:all|visibility:off&style=feature:administrative|element:labels|visibility:off&style=feature:poi|element:labels|visibility:off&style=feature:transit|element:labels|visibility:off'))


edificio_windrose<-tabla[tabla$longitud==lon[2] & tabla$latitud==lat[2], ]%>% mutate(.,longitud=-2.945630-0.00007,latitud=43.257651-0.00005)



ploteo_mapa<- ggmap(edificio_detalle)+coord_fixed()

ploteo_rosa<- edificio_windrose%>%group_by(., longitud,latitud)%>% do(subplots= plot.windrose(., spd = "ws",dir="wd",dirres = 22.5,spdres = 2,spdseq = c(0,1,2,3,5,7,10,15,20), palette = "PuRd"))%>%
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = longitud-0.001,      # change from 1 to other 
                                           y = latitud-0.001,      # values if necessary,
                                           xmax = longitud+0.001,   # depending on the map's
                                           ymax = latitud+0.001))) # resolution.

ploteo_mapa
ggsave(paste0(path_here,"mapa_detalle.tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')


ploteo_mapa+ ploteo_rosa$subgrobs
ggsave(paste0(path_here,"mapa_detallerosa.tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')






plot.windrose(prueba_windrose, spd = "ws",dir="wd",dirres = 22.5,spdres = 2, palette = "PuRd")
ggsave(paste0(path_here,"rosa_conleyenda.tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')




####### ploteo con la rosa elegida

edificio_detalle_lejano<-get_googlemap(center = c(-3.1,43.35),zoom = 11,source= "cloudmap",maptype = c("satellite"),style=c(feature='feature:road|element:all|visibility:off&style=feature:administrative|element:labels|visibility:off&style=feature:poi|element:labels|visibility:off&style=feature:transit|element:labels|visibility:off'))


edificio_windrose_lejano<-tabla[tabla$longitud==lon[2] & tabla$latitud==lat[2], ]


ploteo_mapa<- ggmap(edificio_detalle_lejano)+coord_fixed()

ploteo_rosa<- edificio_windrose_lejano%>%group_by(., longitud,latitud)%>% do(subplots= plot.windrose(., spd = "ws",dir="wd",dirres = 22.5,spdres = 2,spdseq = c(0,1,2,3,5,7,10,15,20), palette = "PuRd"))%>%
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = longitud-360-0.15,      # change from 1 to other 
                                           y = latitud-0.15,      # values if necessary,
                                           xmax = longitud-360+0.15,   # depending on the map's
                                           ymax = latitud+0.15))) # resolution.




ploteo_mapa+ ploteo_rosa$subgrobs+
  geom_point(data = puntos_edificio, aes(lon,lat),size=3, colour="red",alpha=0.7)

ggsave(paste0(path_here,"mapa_detallerosa_lejano.tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')







>>>>>>> c68470beb807859eef3e5b9bfff3b7f944981179
