best <- function(state, outcome) {
  ## Read outcome data
  outcomeN <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(! state %in% outcomeN[,7]){
     stop("invalid state")
  }
  if(outcome == "heart attack"){
    s <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  else if(outcome == "heart failure"){
    s <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  else if(outcome == "pneumonia"){
    s <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  else{
     "invalid outcome"
  }  
  
  ## Return hospital name in that state with lowest 30-day death
  outcomeN[, s] <- as.numeric(outcomeN[, s])
  #print(outcomeN[,s])
  Rates <- subset(outcomeN[,s],outcomeN[, 7] == state)  
  mValues = !is.na(Rates)
  
  lRate <- min(Rates[mValues])
  print(lRate)
  hNames <- subset(outcomeN[,2],outcomeN[, 7] == state)
  #print(hNames)
  hospitalNames <- subset(hNames,Rates == lRate)
  #print(hospitalNames)
  if(length(hospitalNames)>1){
    return(hospitalNames[sort.list(hospitalNames)[1]])
  }
  else{
    return(hospitalNames)
  }
  ## rate
 
}
