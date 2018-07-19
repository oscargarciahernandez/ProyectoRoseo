data_path<- here('data/')
archivos <- list.files(data_path, pattern = 'csv',
                       recursive = TRUE, full.names = TRUE)

revision_tacometro <- function(path_to_csv){
  
  path_to_txt <- str_replace(path_to_csv, pattern = 'csv',
                             replacement = 'txt')
  
  rpm<-as.data.frame(read.table(path_to_txt))[,1]
  rpm_mediana<- median(rpm)
  rpm_mean<- mean(rpm)
  qs <- quantile(rpm, seq(from = 0, to = 1, by = 0.25))
  tapply(rpm, findInterval(rpm, qs), mean)
  
  return(rpm)
  }

Datos_rpm<-lapply(archivos, revision_tacometro)
names(Datos_rpm)<- archivos
head(Datos_rpm)


