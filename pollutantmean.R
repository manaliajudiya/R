pollutantmean <- function(directory, pollutant, id = 1:332) {  
  fileName = ""  
  count = 0
  fmean = numeric(0)
  allValues = c() 
  for(i in id){
    count = count + 1
    fileName = paste(directory,"/",sprintf("%03d",i),".csv",sep="") 
    if(file.exists(fileName)){
      data <- read.csv(fileName)   
      if(pollutant %in% colnames(data)){
        pv = data[pollutant]
        mValues = !is.na(pv)    
        allValues <- c(pv[mValues],allValues)
      }
    }
  }
  if(length(allValues)>0){
    fmean = mean(allValues)
    return(fmean)
    print(round(fmean,digits=3))  
  }
}