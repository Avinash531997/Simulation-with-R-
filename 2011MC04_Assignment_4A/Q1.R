#Avinash Singh
#2011MC04
#Exponential Distribution

Sample_Generation <- function(mew,sigma){
  for(i in 1:n){
    u = runif(1)
    x[i] <- (-sigma)*log(1-u)+mew
  }
  return(x)
}

n = 2000
x = numeric(0)

Generated_Data <- Sample_Generation(0,5)
options(max.print=999999)
print(Generated_Data)


#Computing Mean and Variance 
MEAN <- mean(Generated_Data)
VARIANCE  <-sd(Generated_Data)**2

#PDF Function
Probab_Dist_func<- function(mew,sigma){
  y <-vector()
  y = numeric(0)
  for(i in Generated_Data){
    Temp <- (1/sigma)*exp(-(i-mew)/sigma)
    y <-c(y, Temp)
  }
  return(y)
}

#Computing Vectors op1,op2 
op1<-Probab_Dist_func(0,1)
print(op1)

op2<-Probab_Dist_func(2,1)
print(op2)



print(paste('Mean of the Generated Samples is :',MEAN))
print(paste('Variance of the Generated Samples is :',VARIANCE))

#Plotting 
output <- c("mean 0","mean 2")
clr <- c("orange", "Green")
plot(Generated_Data, op1, col="orange")
points(Generated_Data, op2, col="green")
legend("topright", output, fill=clr)

