pollutantmean <- function(directory, pollutant, id = 1:332){
  
  id_str <- ""
  tot_mean <- 0
  l = length(id)
  s = c()
  
  for(i in id){
    
    if(i < 10){
      id_str <- paste("/00", i, ".csv", sep = "")
    }
    
    else if(i < 100 & i >= 10){
      id_str <- paste("/0", i, ".csv", sep = "")
    }
    
    else{
      id_str <- paste("/", i, ".csv", sep = "")
    }
    
    file_path <- paste(directory, id_str, sep = "")
    data_raw <- read.csv(file_path)
    colnames(data_raw) <- c("Date", "sulfate", "nitrate", "ID")
    
    pol <- data_raw[pollutant]
    pol <- pol[!is.na(pol)]

    
    if(length(pol) > 0){
      s  <- c(s, mean(pol))
    }
    
    else{
      
      l <- l -1
    
    }
    
  }
  
  mean <- mean(s)
  print(mean)
  
}

