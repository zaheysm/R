v1<- c(1,3,5)
v2<- c(7,9,11)
v3<- c(13,15,17)
cbind(v1,v2,v3)

students<- data.frame(
   Name=c("michel A","Jennifer R","Sara B","James B"),
   Gender=c("M","F","F","M"),
   Age=c(18,19,20,22),
   Desgnation= c("CET Student","CP Student","SSN Student","CS Student"),
   NoCourses= c(5,4,"<NA>",3)
)
print(students)