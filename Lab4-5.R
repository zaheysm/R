vecSqr<-function(vector){
  return(vector^2)
}

vec<-c(1,2,3,4)
a <- vecSqr(vec)
print(a)


first3let<-function(string){
  if(nchar(string)<3){
    print("Length of input string is less than 3.")
  }else{
    print(substr(string,1,3))
  }
}

a <- first3let("ab")
a <- first3let("abcdef")


isEven <- function(vector){
  vec<-c()
  for (v in vector){
    
   
      if(is.finite(v)){
       if((v%%2)==0){
          vec<-append(vec,"TRUE")
        }else if((v%%2)!=0) {
          vec<-append(vec,"FALSE")
        }

    }else {
      vec<-append(vec,"NA")
    }

  }
  return(vec)
}


vec<-c(Inf,NA,3,4)
c<-isEven(vec)
cbind(c)

fact<-function(num){

  if(num<=1){
    return(1)
  }
  return(num*fact(num-1))
}

a<-fact(0)
print(a)
b<- fact(3)
print(b)
c<- fact(6)
print(c)
d<- fact(10)
print(d)



lagInter <- function(xVec,yVec){
  ln=length(xVec)
  xK <- readline(prompt=" enter a value: ")
  xK <- as.double(xK)
  cnt=1;
  sum=0;
  mx<-max(xVec)
  mn<-min(xVec)
  if(xK>mn && xK<mx){
    while (cnt<=ln){
      cnt1=1;
      result=1
      while (cnt1<=ln) {
        if(cnt1!=cnt){
          result=(xK-xVec[cnt1])/(xVec[cnt]-xVec[cnt1])*result
          
        }
        cnt1=cnt1+1
        
      }
      sum=sum+(result*yVec[cnt])
      cnt=cnt+1
      
    }
    print(sum)
  }else{
    print("Wrong number entered")
  }

}


xVec<- c(1,1.3,1.6,1.9,2.2)
yVec<- c(0.144,-0.6878,-0.9962,-0.5507,0.3115)

a <- lagInter(xVec,yVec)




