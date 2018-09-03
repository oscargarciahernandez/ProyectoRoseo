 todaslaspaletasposibles<-c("BrBG", "PiYG", "PRGn","PuOr","RdBu","RdGy","RdYlBu", "RdYlGn","Spectral","Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd","PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd")
 oyee_1<- ggmap(prueba_map_con_etiquetas)+coord_fixed()
 
for (i in 1:length(todaslaspaletasposibles)) {
  paleta<- todaslaspaletasposibles[i]
  oyee<- tabla_prueba%>%group_by(., longitud,latitud)%>% do(subplots= plot.windrose(., spd = "ws",dir="wd",dirres = 22.5,spdres = 1,spdseq = seq(0,20,1), palette = paleta))%>%
    mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                             x = longitud-360-0.1,      # change from 1 to other 
                                             y = latitud-0.1,      # values if necessary,
                                             xmax = longitud-360+0.1,   # depending on the map's
                                             ymax = latitud+0.1))) # resolution.
  
  
  path_here<-paste0(here(),"/pruebapaletas/")
  dir.create(path_here)
  tiff(paste0(path_here,"mapa",paleta,".tiff"), width = 7, height =7, units = 'in', res = 300)
  oyee_1+oyee$subgrobs
  dev.off()
}
 