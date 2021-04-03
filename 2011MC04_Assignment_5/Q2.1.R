#2011MC04
#Avinash Singh

n = 5000
x =numeric(0)
X1 =numeric(0)
X2 =numeric(0)
Sample_Generation <- function(){
  for(i in 1:n){
    u = runif(1)
    x[i] <- u
  }
  return(x)
}

Generated_Data1 <- Sample_Generation()
options(max.print=999999)
print(Generated_Data1)

Generated_Data2 <- Sample_Generation()
options(max.print=999999)
print(Generated_Data2)



for(i in 1:length(Generated_Data1))
{
  X1[i]<- ((-2*log(Generated_Data1[i],base=exp(1)))**0.5)*cos(2*3.14*Generated_Data2[i])
  X2[i]<- ((-2*log(Generated_Data1[i],base=exp(1)))**0.5)*sin(2*3.14*Generated_Data2[i])
}

X1
X2

Y1 = dnorm(X1, mean(X1), sd(X1))
# Plot the graph.
plot(X1, Y1)



Y2 = dnorm(X2, mean(X2), sd(X2))
# Plot the graph.
plot(X2, Y2)


#Sample Mean and Variance 
mean(X1)
var(X1)


#Sample Mean and Variance 
mean(X2)
var(X2)
