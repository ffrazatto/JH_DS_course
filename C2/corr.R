corr <- function(directory, threshold = 0){

  ids = 1:332
  correlations <- c()
  
  for(i in ids){
    
    comp <- complete(directory, id = i)
    
    if(comp[2] > threshold){
      

      if(i < 10){
        id_str <- paste("/00", i, ".csv", sep = "")
      }
      
      else if(i < 100 & i >= 10){
        id_str <- paste("/0", i, ".csv", sep = "")
      }
      
      else{
        id_str <- paste("/",i, ".csv", sep = "")
      }
      
      
      file_path <- paste(directory, id_str, sep = "")
      data_raw <- read.csv(file_path)
      
      colnames(data_raw) <- c("Date", "sulfate", "nitrate", "ID")  
  
      data_raw <- data_raw[complete.cases(data_raw),]
      
      x <- data_raw["sulfate"]
      y <- data_raw["nitrate"]
      
      correlations <- c(correlations, cor(x, y))
      
    }
  }
  
  correlations
  
}


