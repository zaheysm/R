
size = readline(prompt ="Please enter the number of data pairs: ")
size = as.integer(size);



readNums <- function(size){
  vec <- c()
  cnt=1
  while(cnt<=size) {
    x <- readline()
    x <- as.double(x)
    vec <- append(vec,x)
    cnt=cnt+1

  }

  return(vec)
}

print("Enter the x-axis values:")
Xaxis <- readNums(size)


print("Enter the y-axis values:")
Yaxis <- readNums(size)



sqrXaxsis <- Xaxis^2
xysum <- Xaxis*Yaxis


a1 <- (size * sum(xysum)- sum(Xaxis)*sum(Yaxis))/(size*sum(sqrXaxsis)-sum(Xaxis)^2)

print(a1)

a0 <- (sum(Yaxis)/size)- (a1*(sum(Xaxis)/size))

print(a0)

cat(a1,"x + ",a0)

yHat <- a1*Xaxis + a0

df <- data.frame("x"=Xaxis,
                 "y"=Yaxis,
                 "y(fitted"=yHat)

print(df)
cnt=1
result=0
while(cnt<=length(Yaxis) ){
  
  result=(Yaxis[cnt]-yHat[cnt])^2+result
  cnt=cnt+1
  
}
cat("The sum of the square of the residuals Sr = ",result)


