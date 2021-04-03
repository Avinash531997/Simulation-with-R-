#2011MC04
#Avinash Singh

#Given
n = 5000
x  =numeric(0)
X1 =numeric(0)
X2 =numeric(0)
X  =numeric(0)

Sample_Generation <- function(){
  for(i in 1:n/2){
    u = runif(1,min=0,max=1)
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

#Combining X1 and X2 into X 
X=c(X1,X2)
length(X)
X
print(mean(X))
print(var(X))


mew <-1
sigma <-2
Expected_Value <- numeric()



XNorm_Input=sigma*X + mew 
print(mean(XNorm_Input))
print(var(XNorm_Input))


#Calculationg Area under the Curve using Simpson's 1/3rd rule
Simpsons_1_3rd <- function(y,h)
{
  n <- length(y) 
  
  if((n-1)%%2 != 0)  
  {
    return(print(paste("Simpson's rule is not applicable here as ",n,"is not even")))
  }
  
  total_sum <- y[1] + y[n]
  
  #Defining sequences 
  itr1 <- seq(2,n-1,2)
  itr2 <- seq(3,n-1,2)
  
  for (i in itr1) 
  {
    total_sum <- total_sum + 4*y[i]
  }
  
  for (i in itr2) 
  {
    total_sum <- total_sum + 2*y[i]
  }
  
  area <- (h/3)*total_sum 
  
  return(area)  
}

#Error_Function
Error_Function<- function(x)
{
  n <- 100
  lb <- 0
  ub <- x
  h <- (ub - lb)/n
  x <- seq(lb , ub, h)
  y <- 2*exp(-x^2)/sqrt(pi)
  
  return(Simpsons_1_3rd(y,h))
}


#CDF_Function()
CDF_Function <- function(x,mew,sigma)
{
  CDF_value <- numeric(0)
  p <- (x-mew)/(sigma*sqrt(2))
  CDF_value <-(1/2)*(1+Error_Function(p))
  return(CDF_value)
}


x2<-XNorm_Input
length(x2)

lower = min(x2)
upper = max(x2)+1 
div<-10
h = (upper-lower)/div

interval <- seq(lower, upper, h) #Generating required intervals
Observed_Value <- rep(0,div)

for(i in 1:div)
{
  for(j in 1:length(x2))
  {
    if(x2[j] >= interval[i] && x2[j] <= interval[i+1])
    {
      Observed_Value[i] <- Observed_Value[i] + 1
    }
  }
}

print(" Observed_Value frequencies are: ")
print(Observed_Value)

print("Sum of Observed_Value frequencies are: ")
print(sum(Observed_Value))

for(i in 1:div)
{
  Expected_Value[i] <- n*(CDF_Function(interval[i+1],mew,sigma) - CDF_Function(interval[i],mew,sigma))
}
print("Expected_Value frequencies are: ")
print(Expected_Value)

print("Sum of Expected_Value frequencies are: ")
print(sum(Expected_Value))

KAI_Square = sum(((Observed_Value-Expected_Value)**2)/Expected_Value)
KAI_Square


#Calculating Critical Value
critical_value <- 16.91898
critical_value

if(KAI_Square > critical_value)
{
  print("Null Hypothesis is rejected")
}else
{
  print("Null Hypothesis is accepted")
}





