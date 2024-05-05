




x <- readline(prompt = "Please enter the value of x:")

x <- as.double(x)

mcSeries <- function(rangeStart,rangeEnd){
  vec <- c(rangeStart:rangeEnd)
  cnt =1
  sum=0
  
  df <- data.frame(matrix(ncol = 3,nrow = 0))
  name <- c("e^(-2x)","Absolute error","Relative error")
  colnames(df) <- name
  sum=0
  while(cnt <= length(vec)){
    
    mc <- (1/factorial(vec[cnt]))*(-2*x)^vec[cnt]
    sum=sum+mc
    
    Ab <- abs(exp(-2*x)-sum)
    Re <- (Ab/exp(-2*x))*100

    df[cnt,"e^(-2x)"]<- mc
    df[cnt,"Absolute error"]<- Ab
    df[cnt,"Relative error"]<- Re
    
    cnt=cnt+1
    
  }

  return(df)
}

df <- mcSeries(1,10)
print(df)

df <- mcSeries(10,100)

plot.ts(df[,"e^(-2x)"])


