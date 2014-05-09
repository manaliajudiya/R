  above <- function(x,n = 10){
    use <- x > n
    x[use] 
    for(i in 1:10){
      print(x[i])
    }
  }