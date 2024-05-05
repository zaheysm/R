x <- 1:10
vec1<-c(x)/10
mat1 <- matrix(c(vec1),nrow=5,ncol=2)
colnames(mat1) <- c("col1","col2")
row.names(mat1) <- c("row1","row2","row3","row4","row5")

mat1["row3","col2"] <- 10

cat("The length of vec1 is",length(vec1),"\n")
print(mat1)

cat("Row 3 is ",mat1["row3",],"\n")
cat("col 2 is ",mat1[,"col2"],"\n")

