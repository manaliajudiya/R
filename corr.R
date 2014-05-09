corr <- function(directory, threshold = 0) {
  
  fileNames = list.files(directory)
  fName = "" 
  allValues = c() 
  crl = ""
   for(f in fileNames){     
     fName = paste(directory,"/",f,sep="") 
     if(file.exists(fName)){       
       data <- read.csv(fName) 
       if(nrow(data[complete.cases(data),]) > threshold){
         dataSulfate = data[complete.cases(data),"sulfate"]
         dataNitrate = data[complete.cases(data),"nitrate"]
         crl <- cor(dataSulfate, dataNitrate, use = "everything", method = c("pearson", "kendall", "spearman"))
         allValues <- c(allValues,crl)
       }     
     }
   }
#   if(length(allValues) == 0){
#     allValues <- cor(NULL, NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
#   }
  return(allValues)
}