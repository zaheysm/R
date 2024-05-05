#Name :Zahi Masarwa
#student number: 040985420
#lab 302

#creates a vector depends on the h sent
createVic <- function(h){
  #empty vector
  vec <- c()
  # vector starting point
  sum=h
  #loop on the vector 
  while (sum<=2) {
    #fill the vector
    vec <- append(vec,sum)
    #addtion tp the next number  
    sum=sum+h
    
  }
  #return the vector 
  return(vec)
  
}

#function to calculate  Euler’s Method
Yi <- function(y,h,x){
  # Euler’s Method
  y1 <- y +(cos(4*x)-2*y)*h
  #return the result of  Euler’s Method
  return(y1)
}

#calculte Euler’s Method and fill it in a vector 
EuMethod <- function(vec,h){
  #counter 
  cnt=1
  #empty vector to return the result 
  resY <- c()
  #loop 
  while(cnt<length(vec)){
    #to handle the first y()
    if(length(resY)==0){
      #fill the vector from the calc in the function
      resY <- append(resY,Yi(3,h,0))
    }else{
      #use the Yi function to calculate Yi+1 and fill it in the vector
      resY <- append(resY,Yi(resY[cnt],h,vec[cnt]))
      #increment the counter 
      cnt=cnt+1
    }
    
   
  }
  #return the vector 
  return(resY)
}

#calculate the exact result by the exact function
#function except vectr and h
ExactT <- function(t,h){
  #counter
  cnt=1
  #create empty vector to return the result
  exactRes <- c()
  #loop 
  while(cnt<=length(t)){
    #do the calculation on exact method 
    ex <- (0.1*cos(4*t[cnt]))+(0.2*sin(4*t[cnt]))+(2.9*exp(-2*t[cnt]))
    #append all the result into the vector 
    exactRes <-append(exactRes,ex)
    #increment the counter 
    cnt=cnt+1
  }
  #return the vector 
  return(exactRes)
  
}

#calculate k1
kj <- function(x,y){
  #k1 calculation by the method 
  k1 <- cos(4*x)-2*y
  #return the result of k1
  return(k1)
}
#calculate k2
kk <- function(x,y,h,k1){
  #to calculate the value of y
  y <- y+h*k1*0.5
  #calculate the x 
  delta <- x+h*0.5
  #calculate k2 result 
  k2 <- cos(delta*4)-2*y
  #return the k2
  return(k2)
}
#calculate k3
kL <- function(x,y,h,k1){
  #to calculate the value of y
  y <- y+h*k1
  #calculate the x 
  delta <- x+h
  #calculate k3 result 
  k2 <- cos(delta*4)-2*y
  #return the k2
  return(k2)
}

#method to calculate the Runge-Kutta 4th Order
rkMethod <- function(vec,h){
  #create the empty vector 
  resY <- c()
  #counter 
  cnt=1
  #loop
  while(cnt<length(vec)){
    #to calculate the y0
    if(length(resY)==0){
      #calculate k1
      k1 <-kj(0,3)
      #calculate k2
      k2 <- kk(0,3,h,k1)
      #calculate k3
      k3 <- kk(0,3,h,k2)
      #calculate k4
      k4 <- kL(0,3,h,k3)
      #calculate y1
      y1 <- 3+(k1+2*k2+2*k3+k4)*h/6
      #append the result of y1 into vector
      resY <- append(resY,y1)
      #to calculate the yi+n
    }else{
      #calculate k1
      k1 <-kj(vec[cnt],resY[cnt])
      #calculate k2
      k2 <- kk(vec[cnt],resY[cnt],h,k1)
      #calculate k3
      k3 <- kk(vec[cnt],resY[cnt],h,k2)
      #calculate k4
      k4 <- kL(vec[cnt],resY[cnt],h,k3)
      #calculate yi+n
      y1 <- resY[cnt]+(k1+2*k2+2*k3+k4)*h/6
      #append the result of yi+n into vector
      resY <- append(resY,y1)
      #increment counter 
      cnt=cnt+1
    }

    
  }

  
  
  return(resY)
}

#function to read the h from the user 
readH <- function(){
  #loop to test if the input is correct
  while(TRUE){
    #prompt the user to input a number 
    h <- readline(prompt = "Choose step size “h” (0.8, 0.2, 0.05)")
    #test if the number as it should
    if(h==0.2 || h==0.05 ||  h==0.8){
      #convert the number to as.numeric from string
      h <- as.numeric(h)
      #return h
      return(h)
    }else{
      #message to let the user to re try
      print("please enter the correct delta")
    }
  }
  
  
  
}
#function to calculate relative error 
Re <- function(vec1,vec2){
  #calculate the relative error and add to the vector 
  vec <- abs(vec1-vec2)/vec1 *100
  #return the vector
  return(vec)
}

#function add every thing to datframe and print it 
printfunc <- function(vec,exvec,esvec,errVec){
  #add every thing to data frame
  data <- data.frame(
    "Time(second)"=vec,
    "Exact Temp(C)"=format(round(exvec,3),nsmall =3),
    "Estimated Temp(C)"=format(round(esvec,3),nsmall=3),
    "Percentage Error(%)"=format(round(errVec,2),nsmall=2)
  )
  print(data)
}


#the main function  that is being called that has the menu 
myODEsolver <- function(){
  #keep looping 
  while(TRUE){
    #message printed to the user
    cat("Choose the method for solving the ODE:
1. Euler’s Method
2. Runge-Kutta 4th Order Method")
    #read option from the user 
    choose <- readline()
    #in case of potion 1
    if(choose=="1"){
      #read h from the function
      h <- readH()
      #create a vector accordingly to by h
      vec <- createVic(h)
      #calculate the exact value 
      exvec <- ExactT(vec,h)
      #calculate the value using Euler’s Method
      euvec <- EuMethod(vec,h)
      #calculate relative error
      rErorr <- Re(exvec,euvec)
      #print function
      printfunc(vec,exvec,euvec,rErorr)
    }else if(choose=="2"){
      #read h from the function
      h <- readH()
      #create a vector accordingly to by h
      vec <- createVic(h)
      #calculate the exact value 
      exvec <- ExactT(vec,h)
      #calculate the value using Runge-Kutta 4th Order Method
      rkVec <- rkMethod(vec,h)
      #calculate relative error
      rErorr <- Re(exvec,rkVec)
      #print function
      printfunc(vec,exvec,rkVec,rErorr)

    }else{
      #print to the user to try again 
      print("option selected is incorrect please tru again")
    }
  }

  
}
#main memthod to start the program 
myODEsolver()

