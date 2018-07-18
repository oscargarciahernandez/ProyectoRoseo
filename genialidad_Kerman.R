library(stringr)
library(magrittr)

data_path<- here('data/')
inputpath2 <- data_path
archivos2 <- list.files(inputpath2, pattern = 'csv',
                        recursive = TRUE, full.names = TRUE)
