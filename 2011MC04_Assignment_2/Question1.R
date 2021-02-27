#Creating Vectors with given values as elements of matrices

vector1 <- c(3,-1,-2,4,1,-2)
vector2 <- c(-7,9,2,4,5,-1)

#Creating Matrix x
x<- matrix(vector1,nrow=2,ncol=3)
x

#Creating Matrix y
y <- matrix(vector2,nrow=3,ncol=2)
y

#Defining a function to Perform Matrix Multiplication
Matrix_Multiplication <- function(x, y){
if(dim(x)[2]!=dim(y)[1]){
print("Multiplication is not Compatible")}
else{
  result <- matrix(0, dim(x)[1], dim(y)[2])  #Creating a Resultant matrix and initializing it to Zero.(NULL MATRIX)
    for(i in seq_along(y[1, ])){
     for(j in seq_along(x[, 1])){
      result[j, i] <- sum(x[j, ] * y[, i])
     }
    }
  result
 }
}

#(I) 
Matrix_Multiplication(x,y)



#Defining Transpose Function
Matrix_Transpose <- function(mt)
{
  mt_row <- length(mt[,1])
  mt_column <- length(mt[1,])
  
  zeroes <- rep(0,times=mt_row*mt_column)
  transpose <- matrix( zeroes,nrow=mt_column,ncol=mt_row)
  
    for (i in 1:mt_row) 
    {
      for (j in 1:mt_column)
      {
        transpose[j,i] <- mt[i,j] 
        
      }
    }
    return(transpose)
}

#(II)Transpose of (AB i.e stored in result) :

result <- matrix(0, dim(x)[1], dim(y)[2]) 
    for(i in seq_along(y[1, ])){
     for(j in seq_along(x[, 1])){
      result[j, i] <- sum(x[j, ] * y[, i])
	}
}

result1 <-Matrix_Transpose(result)



#(III)
#Function to Find  row wise and column wise mean of a matrix
mean_func <-function(x)
{
tot <-0
mavg <-0
for(i in 1:dim(x)[1]){
for(j in 1:dim(x)[2]){
tot=tot+x[i,j]}
mavg=(tot/(dim(x)[2]))
print(paste("Row",i,"Mean is ",mavg))
mavg <-0
tot <-0
}

tot <-0
mavg <-0
for(j in 1:dim(x)[2]){
for(i in 1:dim(x)[1]){
tot=tot+x[i,j]}
mavg=(tot/(dim(x)[1]))
print(paste("Column",j,"Mean is ",mavg))
mavg <-0
tot <-0
}
}


#Function to Find  rowwise and columnwise Standard Deviation of a matrix
sd_func <-function(x)
{
tot <-0
mavg <-0
for(i in 1:dim(x)[1]){
for(j in 1:dim(x)[2]){
tot=tot+x[i,j]}
mavg=(tot/(dim(x)[2]))
tot1 <-0
sdv <-0
for(k in 1:dim(x)[2]){
tot1=tot1+((x[i,k]-mavg)^2)}
sdv=sqrt(tot1/(dim(x)[2]))
print(paste("Row",i,"Standard Deviation is ",sdv))
tot <-0
mavg <-0

}

tot <-0
mavg <-0
for(j in 1:dim(x)[2]){
for(i in 1:dim(x)[1]){
tot=tot+x[i,j]}
mavg=(tot/(dim(x)[1]))
tot1 <-0
sdv <-0
for(k in 1:dim(x)[1]){
tot1=tot1+((x[k,j]-mavg)^2)}
sdv=sqrt(tot1/(dim(x)[1]))
print(paste("Column",j,"Standard Deviation is ",sdv))
tot <-0
mavg <-0
}
}

#For Matrix A 
mean_func(x)
sd_func(x)

#For Matrix B
mean_func(y)
sd_func(y)

#For Matrix AB
mean_func(result)
sd_func(result)

#For Matrix AB Transpose
mean_func(result1)
sd_func(result1)


