rankhospital <- function(state, outcome, num){
  
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
  
  df <- file[file$state == state,]
  
  df_fil <- df[c("hospitalName", "state", outcome)]
  
  df_fil <- df_fil[with(df_fil, order(df_fil[outcome], df_fil["hospitalName"])),]
  
  if(!is.numeric(num)){
    switch (num,
            "best" = p <- 1,
            "worst" = p <- length(df_fil$state),
    )  
  }
  
  else {
    p <- num
  }

  pos <- df_fil[p,]
  
  pos$hospitalName

  df_fil
  
}
