

createVic <- function(h){
  vec <- c()
  sum=h
  while (sum<=6) {
    vec <- append(vec,sum)
    sum=sum+h
    
  }
  
  return(vec)
  
}


Yi <- function(y,h,x){
  y1 <-y+ (0 -y*cos(x)*h)
  return(y1)
}

EuMethod <- function(vec,h){
  
  
  cnt=1
  resY <- c()
  while(cnt<=length(vec)){
    
    if(length(resY)==0){
      resY <- append(resY,Yi(1.241,h,0))
    }else{
      resY <- append(resY,Yi(resY[cnt-1],h,vec[cnt]))
    }
    
    cnt=cnt+1
  }
  return(resY)
}


calcExact <- function(vec){
  cnt=1
  resVec <- c()
  while (cnt<=length(vec)) {
    y <- 0.5*exp(sin(2))* exp(-sin(vec[cnt]))
    resVec <- append(resVec,y)
    cnt=cnt+1
  }
  
  return(resVec)
}

vec1 <- createVic(0.1)
vec2 <- createVic(0.25)
vec3 <- createVic(0.5)

resultvec1 <- EuMethod(vec1,0.1)
resultvec2 <- EuMethod(vec2,0.25)
resultvec3 <- EuMethod(vec3,0.5)

plot(vec1,resultvec1,main="plot for 0.1")
plot(vec2,resultvec2,main="plot for 0.25")
plot(vec3,resultvec3,main="plot for 0.5")

result <- calcExact(vec3)


AeError <-  abs(resultvec3-result)
ReError <- AeError/result*100


data <- data.frame(
  "delta"=vec3,
  "estimate Result"=resultvec3,
  "exact Result" =result,
  "Absulote Error" =AeError,
  "Relative Error" =ReError
)


plot(vec3,result,main="actual Results")

print(data)


