procesador <- function(path_to_csv){
  
  path_to_txt <- str_replace(path_to_csv, pattern = 'csv',replacement = 'txt')
  
  path_to_speed <- str_split(path_to_csv, '/', simplify = TRUE)
  path_to_speed <- path_to_speed[-length(path_to_speed)]
  path_to_speed <- paste(path_to_speed, collapse='/')
  
  path_to_speed_mean <- paste(path_to_speed,"Vviento_mean.txt",sep = '/')
  path_to_speed <- paste(path_to_speed,"Vviento.txt",sep = '/')
  
  if(!file.exists(path_to_speed_mean)){
    
    speed_mean <- mean(read.table(path_to_speed)[,1])
    write.table(speed_mean,file = path_to_speed_mean)
  }else{
    speed_mean<- read.table(path_to_speed_mean)
    if(is.na(speed_mean)){
    speed_mean <- mean(read.table(path_to_speed)[,1])
    write.table(speed_mean,file = path_to_speed_mean) 
    } else{
    speed_mean <- speed_mean[1,1]
    }
  }
  
  
  
  rpm<-colMeans(as.data.frame(read.table(path_to_txt)))
  
  y<-read.table(path_to_csv, sep ="," )
  # El CSV que ofrece el BK es kk esta todo separado por comas
  volts_amps<- colMeans(as.data.frame(cbind(as.numeric(paste0(y[,1],".",y[,2])),
                                    as.numeric(y[,3]+y[,4]*10^(-4)))))
  
  ##introduzco las velocidades del experimento piloto: Vel
  processed_file <-as.data.frame( cbind(rpm[1],speed_mean, volts_amps[1], volts_amps[2]))
  names(processed_file)<- c("RPM","m/s","V", "A")
  
  return(processed_file)
}