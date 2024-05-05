#install.packages("ggplot2")

library(ggplot2)

data(mpg)
head(mpg)

summary(mpg)

myMean <- function(vec){
  sum=0
  for(x in vec){
    sum=x+sum
  }
  return(sum/length(vec))
}

myMedian <- function(vec){
  vec<- sort(vec)
  x <- length(vec)
  x <- as.integer(x)
  if(x%%2==0){
    tmpVec <- c(vec[x/2+1],vec[x/2])
    med <- myMean(tmpVec)
    return(med)
  }else{
    med <- vec[ceiling(x)]
    return(med)
  }
}

myMode <- function(vec){
  
  vecUni <- unique(vec)
  dfVec <- tabulate(match(vec,vecUni))
  vecUni[dfVec==max(dfVec)]
  
  
}

myStaDev <- function(vec,bol){
  mean <- myMean(vec)
  sum=0
  for (x in vec){
    sum <- sum+(x-mean)^2
  }
  if(bol){
    return(sqrt(sum/length(vec)))
  }else{
    return(sqrt(sum/(length(vec)-1)))
  }
}

myVar <- function(vec,bol){
  var <- myStaDev(vec,bol)
  return(var^2)
}


set.seed(142)
mynormalData <- rnorm(10000, mean=50, sd=8)
hist(mynormalData, breaks=80)
mean(mynormalData)
AcSd<-sd(mynormalData)
print(AcSd)
myMean(mynormalData)
mySd<-myStaDev(mynormalData,TRUE)
print(mySd)
cat("absolute error for SD is",abs(AcSd-mySd),"\n")
cat("relative error for SD is",abs(AcSd-mySd)/AcSd*100,"%\n")

vecCty <- unlist(mpg[,"cty"], use.names = FALSE)


myMean(vecCty)
myMedian(vecCty)
myMode(vecCty)
myStaDev(vecCty,TRUE)
myVar(vecCty,TRUE)

#tmpvec1 <- c(1,1,1,2,2,2,3,5,4,8,6,5,4,9)
#myMode(tmpvec1)

mean(vecCty)
median(vecCty)
sd(vecCty)


BMI <- data.frame(
  gender=c("male","male","female","male","female","female"),
  height=c(81,93,78,100,92,75),
  weight=c(152,171.5,165,140,192.1,180.2),
  Age=c(42,38,26,52,18,23)
)


print(BMI)


HeightM <- mean(BMI[,"height"])
HeightSD <- sd(BMI[,"height"])

print(HeightM)
print(HeightSD)

weightM <- mean(BMI[,"weight"])
weightSD <- sd(BMI[,"weight"])

print(weightM)
print(weightSD)

Z85 <- (85-HeightM)/HeightSD
print(Z85)


Z166 <- (166-weightM)/weightSD
print(Z166)


AgeM <- mean(BMI[,"Age"])
AgeSD <- sd(BMI[,"Age"])
print(AgeM)
print(AgeSD)


Z35 <-(35-AgeM)/AgeSD
Z45 <-(45-AgeM)/AgeSD
print(Z35)
print(Z45)

cat("probability that height is less than 85 is",0.4404*100,"\n")
cat("probability that weight is greater than 166 is",(1-0.4840)*100,"\n")
cat("probability that Age is between than 35 to 45 is",(0.8159-0.5557)*100,"\n")


pnorm(85,mean=HeightM,sd=HeightSD)
pnorm(166,mean=weightM,sd=weightSD)

pnorm(35,mean=AgeM,sd=AgeSD)
pnorm(45,mean=AgeM,sd=AgeSD)



