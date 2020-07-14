best <- function(state, outcome){
  
  file <- read.csv("outcome-of-care-measures.csv")
  
  cond <- paste("Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep = "")
  
  
  st <- file["State"]
  out <- as.numeric(file[,cond])
  h_name <- file["Hospital.Name"]
  
  df <- cbind(h_name, out, st)
  
  df <- df[df$State == state,]
  
  m <-which.min(df$out)
  
  b <- df[m, "Hospital.Name"]
  b
}