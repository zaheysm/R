#Name :Zahi Masarwa
#student number: 040985420
#lab 302


#######Task 1: CSV Files ########
#read the file 
CarsList <- read.csv(file='C:/Users/zahey/Documents/College/R/Assigment 1/assignment1.csv',sep=";")
#shot the top 10 rows in the file
head(CarsList,10)

#create an ew data frame minus the type rows
CarsList <- CarsList[-c(1),]
#print the number of rows
cat("number of rows is :",nrow(CarsList),"\n")
#print the number of columns 
cat("number of coulmns is :",length(CarsList))

#see the types of the columns 
sapply(CarsList,class)

#change the types of the columns 
CarsList[,"MPG"]=as.double(CarsList[,"MPG"])
CarsList[,"Cylinders"]=as.integer(CarsList[,"Cylinders"])
CarsList[,"Displacement"]=as.double(CarsList[,"Displacement"])
CarsList[,"Horsepower"]=as.double(CarsList[,"Horsepower"])
CarsList[,"Weight"]=as.double(CarsList[,"Weight"])
CarsList[,"Acceleration"]=as.double(CarsList[,"Acceleration"])
CarsList[,"Model"]=as.integer(CarsList[,"Model"])
CarsList[,"Origin"]=as.factor(CarsList[,"Origin"])


#print the mpg between 22 and 26
print(subset(CarsList,MPG>=22 & MPG<=26))

#write.csv(subset(CarsList,MPG>=22 & MPG<=26),file="~\\College\\R\\Assigment 1\\MPG.csv",row.names = FALSE,sep=";")
#ouptu the file of horsepower more than 150
write.csv(subset(CarsList,Horsepower>150),file="~\\College\\R\\Assigment 1\\Horsepower.csv",sep=";",row.names = FALSE)
#ouptu the file of Acceleration less than 14.5
write.csv(subset(CarsList,Acceleration<14.5),file="~\\College\\R\\Assigment 1\\Acceleration.csv",sep=";",row.names = FALSE)
#create a new data frame not include 0
CarlistP <- subset(CarsList,MPG>0)
#show the histogram include 10 breaks and color red the name of x axis and main name of the graph 
hist(CarlistP[,"MPG"], breaks = 10,main="MPG Histogram",xlab ="MPG", col = "RED")



#########Part II: Vectors, Calculations, Series #########
#set the seed to 90
set.seed(90)
#create two empty vectors 
xVec <- c()
yVec <- c()

#counter to count for the loop
cnt=1;
#loop to loop till 50
while(cnt<=50){
  xVec[cnt]=sample.int(250)#enetring a random numebr
  yVec[cnt]=sample.int(250)#enetring a random numebr
  cnt=cnt+1 #incrament thecounter by 1 
}
#create new vector for the total 
tVec <- c()
#counter to count for the loop
cnt=1;
#loop to loop till 49
while(cnt<=49){
  #calculate the diffrence 
  tVec<- append(tVec,as.integer(xVec[cnt])-as.integer(yVec[cnt+1]))
  cnt=cnt+1
}
#create the matrix
mat <- matrix(tVec,nrow = 7,ncol = 7)
#print the matrix
print(mat)
#create new vector for the sin cos
sincosVic <- c()

#counter to count for the loop
cnt=1
#loop to loop till length(yVec)
while(cnt<length(yVec)){
  #change it to integer from char
  y<- as.integer(yVec[cnt])
  #change it to integer from char
  x<- as.integer(xVec[cnt+1])
  #calculate the sin and cos
  sincosVic <- append(sincosVic,cos(y)/sin(x))
  #incrament the counter 
  cnt=cnt+1
}
#print the vector 
print(sincosVic)
#counter to count for the loop
cnt=1
#create a variable for the sum 
sum=0
#loop to loop till length(yVec)
while(cnt<length(xVec)){
  #change it to integer from char into x1
  x1<- as.integer(xVec[cnt+1])
  #change it to integer from char into x
  x<- as.integer(xVec[cnt])
  #change it to integer from char into y
  y<- as.integer(yVec[cnt])
  #do the calculation of the eqation 
  sum=exp(-x1)/(x+5)*log(y)+sum
  #incrament the counter 
  cnt=cnt+1
}
#print sum 
print(sum)


#####Part III BONUS: Float-to-Binary Conversion########
#function the will return the number as binary vector
toBinary <- function(x){
  #craete an empty vector
  binVicX <- c()
  #loop as long the number bigger or equal to 0.5
  while(x>=0.5){
    #if the modelo of x = 0
    if(x%%2==0){
      #add 0 to the the vector 
      binVicX<-append(binVicX,0)
    }else{
      #add 1 to the the vector 
      binVicX<-append(binVicX,1)
    }
    #calculate x to devide by 2 and only take the number before the point
    x=trunc(x/2,1)
  }
  #return the vector in reverse order 
  return(rev(binVicX))
}
#function to calculate the ieee754
ieee754 <- function(d){
  #variable will be use for the sign
  s=0
  #condition if the number is not a number 
  if(!is.double(d)){
    print("is not a number")
    return()
  }
  #if the number smaller than 0
  if(d<0){
    s=1
  }
#take the abstract of the number before the point
x<-abs(trunc(d, 1))
#take the abstract of the number after the point
y <- abs(d) - abs(trunc(d))
#get the result of the binary number 
binVicX <- toBinary(x)
#craete an empty vector
binVicY <- c()
#loop to calcultae the binary after the point
while(y<1){
  #if multiple by 2 bigger than 1
  if(y*2>=1){
    #add 1 to the vector 
    binVicY<-append(binVicY,1)
  }else{
    #add 0 to the vector 
    binVicY<-append(binVicY,0)
  }
  #multiple y by 2 for the loop
  y=y*2
  #if bigger than 1 , just take what is bellow 1
  if(y>1){
    y=y-1
  }
}

#varaible for the exponet 
Exp=0;
#condition to get the length of binvicx vector if equal 0 in case there is smaller than 1
if (length(binVicX)==0){
  cnt=0
  #count the the 0 in after decimal point to know the exponent
  for (i in binVicY){
    
    if(i==0){
      cnt=cnt+1
    }else{
      break
    }
  }
  Exp=127-cnt-1
}else{
  #set the exponent when it's bigger than 1
  Exp=length(binVicX)-1+127
}
  #change the exponent to binary
  Exponet<- toBinary(Exp)
  #if the exponent length less than 0 make sure every thing is 0
  if(length(Exponet)<8){
    Exponet <- c(0,Exponet)
  }
  #in case exponent less than 127 or equal some manuplations needed to be done  
  if(Exp<=127){
    #if the length of decimal equal to 1 and there no number after decimal point the mantisa will be 0
    if(length(binVicY)==1 && length(binVicX)==0){
      Mantissa <- c(0)
    }else{
      #in case the length of the decimal point is 1 only take what after the decimal point 
      if(length(binVicX)==1){
        Mantissa <- c(binVicY[1:length(binVicY)])
      }else if(length(binVicX)==0) {
        #if it's not start from the second digit 
        Mantissa <- c(binVicY[2:length(binVicY)])
      }
      
    }
    
  }else{
    #print the mentisa in normal case 
    Mantissa <- c(binVicX[2:length(binVicX)],binVicY[1:length(binVicY)])
  }
  # if the mentisa less than 23 make sure that it's only 23
  if(length(Mantissa)<23){
    cnt <- length(Mantissa)
    while (cnt<=23) {
      Mantissa<-append(Mantissa,0)
      cnt=cnt+1
    }
  }else{
    Mantissa <- Mantissa[1:23]
  }
  #output the results 
  cat("Input: " ,d  ,"IEEE-754, 32-bit floating point number representation:\n")
  cat("Sign = ",s," \n")
  cat("Mantissa (23 bits) = ", gsub(", ","",toString(Mantissa)),"\n")
  cat("Exponent (8 bits) = ",  gsub(", ","",toString(Exponet)),"\n")

}


ieee754(-103.5)
