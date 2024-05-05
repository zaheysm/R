examDF<-data.frame(
  name=c("Anastasia","Dima","Katherine","James","Emily","Michael","Matthew","Laura","Kevin","Jonas"),
  score=c(12.5,9.0,16.5,12.0,9.0,20.0,14.5,13.5,8.0,19.0),
  attempts=c(1,3,2,3,2,3,1,1,2,1),
  qualify=c("yes","no","yes","no","no","yes","yes","no","no","yes")
)

examRES<-examDF[,c("name","score")]

examQual <- subset(examDF,qualify=="yes")

print(examDF)

print(examRES)

print(examQual)