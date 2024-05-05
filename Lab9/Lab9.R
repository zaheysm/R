

# readd libary to read excel files 
library("readxl")
setwd("C:/Users/zahey/Documents/College/R/Lab9")

data <- as.data.frame(read_excel("data.xlsx"))

head(data)

xVec <- data[,1]
yVec <- data[,2]

plot(xVec,yVec,xlab="time",ylab="Distence")
funcfirstdev <- function(){
  firstDev <- c()
  cnt=2
  dev <- (yVec[cnt]-yVec[cnt-1])/ (xVec[cnt]-xVec[cnt-1])
  firstDev <- append(firstDev,dev)
  while (cnt<length(xVec)){
    
    dev <- (yVec[cnt+1]-yVec[cnt-1])/ (xVec[cnt+1]-xVec[cnt-1])
    firstDev <- append(firstDev,dev)
    cnt=cnt+1
  }
  
  dev <- (yVec[cnt]-yVec[cnt-1])/ (xVec[cnt]-xVec[cnt-1])
  firstDev <- append(firstDev,dev)
  return(firstDev)
}


funcSecondDev <- function(){
  
  secondDev <- c()


  cnt=2

  while (cnt<length(xVec)){
    
    dev <- (yVec[cnt+1]-2*yVec[cnt]+yVec[cnt-1])/(xVec[cnt+1]-xVec[cnt])^2
    secondDev <- append(secondDev,dev)
    cnt=cnt+1
  }

  return(secondDev)
}

firstDev <- funcfirstdev()
firstDev <- firstDev*1000

secondDev <- funcSecondDev()*1000

plot(xVec,firstDev,xlab="time",ylab="velocity")


plot(xVec[2:length(xVec)-2],secondDev,xlab="time",ylab="acceleration ")

