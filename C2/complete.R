complete <- function(directory, id = 1:332)
{
  c_id <- id
  c_nobs <- replicate(0, length(id))
  k <- 1
  
  
  id_str <- ""
  #l = length(id)
  
  for(i in id){
    
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
    
    c_nobs[k] <- nrow(data_raw)
    
    k <- k + 1
    
  } 
  
  r <- data.frame(cbind(c_id, c_nobs))

  colnames(r) <- c("id", "nobs")
  
  r  
  
}


