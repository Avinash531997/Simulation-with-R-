#2011MC04 Avinash Singh
#Given: 
n<-2000
x = numeric(0)

Cauchy_Sample_Generator <- function(sigma,mew)
{
  for(i in 1:n)
  {
    #U(0,1) uniform distribution
    u = runif(1,min = 0,max = 1)  
    x[i] <- mew + sigma*tan(pi*(u-(1/2)))
  }
  return(x)
}

CDF_Function <- function(x, sigma, mew)
{
  cdf <- (1/pi)*atan((x-mew)/sigma) +(1/2)
  return(cdf)
}


Generated_Data1 <- Cauchy_Sample_Generator(5,3)

a = min(Generated_Data1) - 1
b = max(Generated_Data1) + 1
h = (b-a)/10

interval <- seq(a, b, h)
interval

Observed_Value <- c(0,0,0,0,0,0,0,0,0,0)

for(i in 1:10)
{
  for(j in 1:n)
  {
    if(Generated_Data1[j] > interval[i] && Generated_Data1[j] <= interval[i+1])
    {
      Observed_Value[i] <- Observed_Value[i] + 1
    }
  }
}
sum(Observed_Value)
Observed_Value

Expected_Value <- numeric()

for(i in 1:10)
{
  Expected_Value[i] <- n*(CDF_Function(interval[i+1],5,3)- CDF_Function(interval[i],5,3))
}
print('Expected values are:')
print(Expected_Value)

print('Sum of Expected values are:')
print(sum(Expected_Value))

KAI_Square = sum(((Observed_Value-Expected_Value)**2)/Expected_Value)
KAI_Square
