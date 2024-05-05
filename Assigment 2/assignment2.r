#Name :Zahi Masarwa
#student number: 040985420
#lab 302

#install.packages("readxl")

# readd libary to read excel files 
library("readxl")
#set the main folder to find where the file found 
setwd("C:/Users/zahey/Documents/College/R/Assigment 2")

#function to input the name of the file and read it 
readFile <- function(){
  #prompt the user to inout name of the file 
  fileName=readline(prompt = "Please enter the name of the file to open: " )
  #test if the file not exist try again
  while(!file.exists(fileName)){
    fileName=readline(prompt = "File name Wrong ,Please enter the name of the file to open: " )
  }
  #input the file into data frame 
  COVID19_data <- read_excel(fileName)
  #print the summury of the file from coulmn 3 to the end
  print(summary(COVID19_data[,3:13]))
  #return data 
  return(COVID19_data)
}


#read the axis of date and convert it to number so it will readble  
readXaxis <- function(data){
  #empty vector 
  vec <- c()

  #tmp vec for the date   
  #date converted to number 
  dt <- as.numeric(as.Date(data,"%Y-%m-%d"))
  #append number date  to vector 
  vec <- append(vec,dt)
  #return vector 
  return(vec)
}


#function to build the best function
bestFit <- function(COVID19_data){
  #reading the data from the x axis 
  xAxis <-readXaxis( COVID19_data[,"date"])
  #reading the data from the y axis 
  yAxis <- COVID19_data[,"total_vaccinations"]

  #ln for the y axis
  yAxis <- log(yAxis)
  #square of x axis
  sqrXAxis <- xAxis^2

  #the sum of x+y axis 1
  
  xysum <- yAxis*xAxis

  #calculate the size of date or is the n
  size <- length(COVID19_data[,"total_vaccinations"])
  
  #calculate the a1
 
  d <- ((size * sum(xysum)) -(sum(xAxis)*sum(yAxis)))
  n <- ((size*sum(sqrXAxis))-sum(xAxis)^2)
  a1 <- d/n


  #calculate the a0
  a0 <- (sum(yAxis)/size)- (a1*(sum(xAxis)/size))
  
  #calculate the a
  a=exp(a0)
  #print the equasion 
  cat(a,"x","e^",a1,"t","\n")
  
  #put a and b in the vector
  vec <- c(a,a1)
  
  #return vector of a and b 
  return(vec)

}
#check date that is valid return false if it's not 
checkDate <- function(date,maxdate,mindate){

  #split the date into day month and year 
  newDate <- unlist(strsplit(date,split ='/'))

  date <- as.Date(ISOdate(as.numeric(newDate[3]),as.numeric(newDate[2]),as.numeric(newDate[1])))
  if(date < as.Date(maxdate,"%Y-%m-%d") & date > as.Date(mindate,"%Y-%m-%d")){
    
  }else{
    return(FALSE)
  }
  
 
  #if the day bigger than it should be or smaller
  if (as.numeric(newDate[1])<0 || as.numeric(newDate[1])>32){
    return(FALSE)
  }else if(as.numeric(newDate[2]==2)){
    if (as.numeric(newDate[1])<0 || as.numeric(newDate[1])>29){
      return(FALSE)
    }
  }
  #if the month bigger than it should be or smaller
  if (as.numeric(newDate[2])<0 || as.numeric(newDate[2])>12){
    return(FALSE)
  }
  #if the year bigger than it should be or smaller
  if (as.numeric(newDate[3])<0 || as.numeric(newDate[2])>9999){
    return(FALSE)
  }
  #return true
  return(TRUE)
  
  }

#function to check the pattern  if they enterd as tehy should 
checkPatteren <- function(date){
  #make that is 10 chars entered if not return false
  if(nchar(date)!=10 ){
    return(FALSE)
  }
  #split the string by chars 
  data <- unlist(strsplit(date, split =  ""))
  #make sure that the 3rd and 6th char is /  
  if(data[3]!="/" || data[6]!="/"){
    return(FALSE)
  }
  return(TRUE)
}

#function to plot date range and save to pdf
plotdata <- function(orgData,abdata){
  size<- length(unlist(orgData[,"date"]))
  cnt=1
  sum=0
  estTv <- c()
  while (cnt<=size) {

    x <- abdata[1]*exp(abdata[2]*as.numeric(as.Date(orgData[cnt,"date"],"%Y-%m-%d")))
    sum=sum+x
    estTv <- append(estTv,x)
    cnt=cnt+1
  }
  
  
  dataplot <- data.frame(
    date=c(orgData[["date"]]),
    total_vaccinations=c(orgData[["total_vaccinations"]]),
    bestFitVac=c(estTv)
  )
  
  plot(dataplot[,"date"], dataplot[,"total_vaccinations"], col="blue", ylab="total vaccinations",xlab="Date", lty=1,main="Total vaccination by fit and original")
  points(dataplot[,"date"], dataplot[,"bestFitVac"], col="green")
  

  pdf(file = "total_vacc.pdf",   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4)
  plot(dataplot[,"date"], dataplot[,"total_vaccinations"], col="blue", ylab="total vaccinations",xlab="Date", lty=1,main="Total vaccination by fit and original")
  points(dataplot[,"date"], dataplot[,"bestFitVac"], col="green")
  
  dev.off()
}


#function that hold the menus
menu <- function(){
  #for the loop to exit when it's done
  done <- TRUE
  #loop
  while(done){
    #print the munue for the user 
    cat("menu \n","1. Exponential Fit\n","2. Quit")
    #prompt the user to select an option 
    choose <- readline(prompt = "please select an option: ")
    # if the user choose option 1
    if(choose==1){
      #read the file 
      COVID19_data <- readFile()
      COVID19_data <- as.data.frame(COVID19_data)
      
      #read the date start 
      date1= readline(prompt = "Please enter the start date (dd/mm/yyyy):")
      #read the date end 
      date2= readline(prompt = "Please enter the end date (dd/mm/yyyy):")
      #check if the date pattern is corect and end the program 
      if(checkPatteren(date1)==FALSE || checkPatteren(date2)==FALSE){
        print("error date Patteren ")
        break
      }
      maxdate <- max(COVID19_data[,"date"])
      mindate <- min(COVID19_data[,"date"])
    
      #check date if the date is correctly input
      if(checkDate(date1,maxdate,mindate) && checkDate(date2,maxdate,mindate)){
        #split the string to read day month and year 
        newDate1 <- unlist(strsplit(date1,split ='/'))
        newDate2 <- unlist(strsplit(date2,split ='/'))
        #make date1 and date2 to date 
        date1 <- as.Date(ISOdate(as.numeric(newDate1[3]),as.numeric(newDate1[2]),as.numeric(newDate1[1])))
        date2 <- as.Date(ISOdate(as.numeric(newDate2[3]),as.numeric(newDate2[2]),as.numeric(newDate2[1])))
        #check if the date is under 60 days break the program 
        if(date2-date1<60){
          print("date diff under 60 days")
         
        }
        
      }else{
        print("Date input was invalid")
        break
      }
      #use the dates needed and put the data frame in new dataframe
      vecdate <- subset(COVID19_data,as.Date(COVID19_data[,2],"%Y-%m-%d")>=date1 & as.Date(COVID19_data[,2],"%Y-%m-%d")<=date2)
      
      #find the equasion and return A and B
      ab <- bestFit(vecdate)
      
      #loop for menu 2
      while(done){
        #print menu for the user to choose 
        cat("menu \n","1. Extrapolation\n","2. Main Menu")
        #prompt the user what to choose 
        choose2 <- readline(prompt = "please select an option: ")
        #if the user choose 1 option
        if(choose2==1){
          #prompt the user to input a date 
          date3= readline(prompt = "Please enter the date to extrapolate to (dd/mm/yyyy):")
          #check if the date pattern is corect and end the program 
          if(checkPatteren(date3)==FALSE){
            print("Date input was invalid")
          }
           #check date if the date is correctly input
          if(checkDate(date3,maxdate,mindate)){
            newDate3 <- unlist(strsplit(date3,split ='/'))
            #split the string to read day month and year 
            date3 <- as.Date(ISOdate(as.numeric(newDate3[3]),as.numeric(newDate3[2]),as.numeric(newDate3[1])))
            #according to a and b calculate the funcation 
            estTv <- ab[1]*exp(ab[2]*as.numeric(date3))
            #print the result
            cat("best fit for the date chosen is :",estTv,"\n")
            plotdata(vecdate,ab)
          }else{
            print("Date input was invalid")
          }
          #if choose 2 go back to main menu
        }else if(choose2==2){
          
          break
          
        }
        #if choose any other number will ptint inccorect input 
        else{
          print("The option choosen is incorrect ")
        }
      }
      
    }
    #if choose 2 quit the program
    else if(choose==2){
      break
    }
    #if choose any other number will ptint inccorect input 
    else{
      print("The option choosen is incorrect ")
    }
  }
    
}


#start the program 
menu()
