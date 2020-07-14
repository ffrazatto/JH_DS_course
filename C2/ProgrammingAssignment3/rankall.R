rankall <- function(outcome, num = "best"){
  
  file <- read.csv("outcome-of-care-measures.csv",
                   na.strings = "Not Available",
                   stringsAsFactors = FALSE)
  
  file <- file[,c(2,7,11,17,23)]
  
  r <- 0
  
  switch(outcome,
         "heart attack" = r <- 11,
         "heart failure" = r <- 17,
         "pneumonia" = r <- 23)
  
  
  colnames(file) <- c("hospitalName",
                      "state",
                      "heart attack",
                      "heart failure",
                      "pneumonia")
  
  file <- file[complete.cases(file),]
  
  #df <- file[file$state == state,]
  df_fil <- file[c("hospitalName", "state", outcome)]
  
  df_fil <- df_fil[with(df_fil, order(df_fil["state"],
                                      df_fil[outcome],
                                      df_fil["hospitalName"])),]
  

  
  groups <-split(df_fil, df_fil$state)
  # groups
  
  
  s <- c()
  hn <- c()
  
  for(g in groups){

    if(!is.numeric(num)){
      switch (num,
              "best" = p <- 1,
              "worst" = p <- length(g$state),
      )  
    }
    
    else {
      p <- num
    }
    
        
    s <- c(s, g$state[1])
    hn <- c(hn, g$hospitalName[p])
    
  }
  
  a <- as.data.frame(cbind(s,hn))
  colnames(a) <- c("state", "hospital")
  
  a
  df_fil
  
}