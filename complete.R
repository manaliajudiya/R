complete <- function(directory, id = 1:332) {
  fileName = ""  
  count = 0
  fmean = numeric(0)
  allValues = c() 
  op = data.frame(id = id, nobs = rep(0))
  for(i in id){
    count = count + 1
    fileName = paste(directory,"/",sprintf("%03d",i),".csv",sep="") 
    if(file.exists(fileName)){
      data <- read.csv(fileName)   
      op[count,1] = i
      op[count,2] = nrow(data[complete.cases(data),])      
    }
  }
  print(op)
}