install.packages("ggmap")
library(ggmap)

get_map()
sum(lon)/length(lon)-360
prueba_map<-get_map(location = c((sum(lon)/length(lon)-360),(sum(lat)/length(lat))))
ggmap(prueba_map)
