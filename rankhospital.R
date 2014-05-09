rankhospital <- function(state, outcome, num = "best") {
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
    stop("invalid outcome")
  }  
  
  ## Return hospital name in that state with lowest 30-day death
  outcomeN[, s] <- as.numeric(outcomeN[, s])
  #print(outcomeN[,s])
  
  mValues = !is.na(outcomeN[,s])
  
  outcomeN <- outcomeN[mValues,]
  rank <- subset(outcomeN,outcomeN[, 7] == state,select = c(s,'Hospital.Name'))
  rank <- rank[order(rank[,1],rank[,2]),]
  if( num == "best"){
    return(rank[1,2])
  }
  else if( num == "worst"){
    w <- tail(rank)
    w <- w[order(-w[,1],w[,2]),]
    return(w[1,2])
  }
  else{   
    
    return(rank[num,2])
  }
  ## rate
  
}