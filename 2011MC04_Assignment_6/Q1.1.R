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



#CDF of Exponential distribution
CDF_function<- function(x,sigma,mew)
{
  cdf <- 1- exp(-(x-mew)/sigma)
  return(cdf) 
}



Generated_Data1 <- Sample_Generation(1,0)

a = 0
b = max(Generated_Data1)+1
h = (b-a)/10

# Creating Intervals
interval <- seq(a, b, h) 
interval

Observed_Values <- c(0,0,0,0,0,0,0,0,0,0) 

#Maintaining the Frequency !
for(i in 1:10)
{
  for(j in 1:n)
  {
    if(Generated_Data1[j] > interval[i] && Generated_Data1[j] <= interval[i+1])
    {
      Observed_Values[i] <- Observed_Values[i] + 1
    }
    
  }
}


print('Observed values : ')
print(Observed_Values)

print('Sum of Observed values: ')
print(sum(Observed_Values))

for(i in 1:10)
{
  Expected_Value[i] <- n*(CDF_function(interval[i+1],1,0)- CDF_function(interval[i],1,0))
}

print('Expected_Values  are: ')
print(Expected_Value)

print('Sum of Expected_Values  are: ')
print(sum(Expected_Value))

KAI_square = sum(((Observed_Values-Expected_Value)**2)/Expected_Value)
KAI_square

