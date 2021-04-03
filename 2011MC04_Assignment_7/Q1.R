#2011MC04
#Avinash Singh

n = 5000
x =numeric(0)
X =numeric(0)
Sample_Generation <- function(mew,sigma){
  for(i in 1:n){
    u = runif(1,min=0,max=1)
    x[i] <- u
  }
  return(x)
}


Generated_Data1 <- Sample_Generation(0,1)
options(max.print=999999)
#print(Generated_Data1)

Generated_Data2 <- Sample_Generation(0,1)
options(max.print=999999)
#print(Generated_Data2)

Generated_Data3 <- Sample_Generation(0,1)
options(max.print=999999)
#print(Generated_Data3)



for(i in 1:length(Generated_Data1))
{
  if(Generated_Data1[i]<=0.5)
  {
    X[i]<-Generated_Data2[i]
  }
  else
  {
    X[i]<-max(Generated_Data2[i],Generated_Data3[i])
  }
}

#Sample Points 
X

#CDF_Function
CDF_function<-function(x)
{
  a=0.5
  return(a*x+(1-a)*x*x)
}

#Sample Mean and Variance 
mean(X)
var(X)


al = 0
b = max(X)
h = (b-al)/10

# Creating Intervals
interval <- seq(al, b, h) 
interval

Observed_Values <- c(0,0,0,0,0,0,0,0,0,0) 


#Maintaining the Frequency !
for(i in 1:10)
{
  for(j in 1:n)
  {
    if(X[j] > interval[i] && X[j] <= interval[i+1])
    {
      Observed_Values[i] <- Observed_Values[i] + 1
    }
    
  }
}
print('Observed values : ')
print(Observed_Values)

print('Sum of Observed values: ')
print(sum(Observed_Values))


Expected_Value <- numeric() 


for(i in 1:10)
{
  Expected_Value[i] <- n*(CDF_function(interval[i+1])- CDF_function(interval[i]))
}

print('Expected_Values  are: ')
print(Expected_Value)

print('Sum of Expected_Values  are: ')
print(sum(Expected_Value))

KAI_square = sum(((Observed_Values-Expected_Value)**2)/Expected_Value)
KAI_square

critical_value=16.92
if(KAI_square<critical_value){
  print("Null Hypothesis Accepted")}else{
  print("Null Hypothesis Rejected")}






#Manualy Distribution Mean was found to be : (4-a)/6.
#at a=0.5, Mean = 0.88