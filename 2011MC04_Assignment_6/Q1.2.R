#2011MC04 Avinash Singh

#Given:
n = 2000 

x = numeric(0)
Expected_Value <- numeric() 

Sample_Generation <- function(sigma,mew)
{
  for(i in 1:n)
  {
    #U(0,1) uniform distribution
    u = runif(1,min = 0,max = 1) 
    x[i] <- (mew - sigma*log(1-u))
  }
  return(x) 
}

CDF_Function<- function(x,sigma,mew)
{
  cdf <- 1- exp(-(x-mew)/sigma)
  return(cdf) 
}

#Generating 2000 sample points which follows exponential distribution for mean greater than 0
generated_data_2 <- Sample_Generation(1,1)

a = 0
b = max(generated_data_2)+1
h = (b-a)/10

#Intervals
interval <- seq(a, b, h)
interval

# Observed_Value values
Observed_Value <- c(0,0,0,0,0,0,0,0,0,0)

for(i in 1:10)
{
  for(j in 1:n)
  {
    if(generated_data_2[j] > interval[i] && generated_data_2[j] <= interval[i+1])
    {
      Observed_Value[i] <- Observed_Value[i] + 1
    }
  }
}

print(' Observed values are:')
print(Observed_Value)

print(' Sum of Observed values are:')
print(sum(Observed_Value))

Expected_Value <- numeric() #Expected_Value values

for(i in 1:10)
{
  Expected_Value[i] <- n*(CDF_Function(interval[i+1],1,1)- CDF_Function(interval[i],1,1))
}

print(' Expected_Value values are:')
print(Expected_Value)

print(' Sum of exected values are:')
print(sum(Expected_Value))

KAI_Square = sum(((Observed_Value-Expected_Value)**2)/Expected_Value)
KAI_Square
