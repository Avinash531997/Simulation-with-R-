#2011MC04
#Avinash Singh

n = 5000
x =numeric(0)
X =numeric(0)
Sample_Generation <- function(mew,sigma){
  for(i in 1:n){
    u = runif(1)
    x[i] <- u
  }
  return(x)
}

Generated_Data1 <- Sample_Generation(0,1)
options(max.print=999999)
print(Generated_Data1)

Generated_Data2 <- Sample_Generation(0,1)
options(max.print=999999)
print(Generated_Data2)

Generated_Data3 <- Sample_Generation(0,1)
options(max.print=999999)
print(Generated_Data3)



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

X

#PDF Function
Probab_Dist_func<- function()
  {
  a=0.5
  y <-vector()
  y = numeric(0)
  for(i in X)
    {
    Temp <-a+2*(1-a)*i
    y <-c(y, Temp)
   }
  return(y)
}
op1<-Probab_Dist_func()
print(op1)

#Sample Mean and Variance 
mean(X)
var(X)

#Manualy Distribution Mean was found to be : (4-a)/6.
#at a=0.5, Mean = 0.88