#Avinash Singh 2011MC04

#We will first declare the Function for:
#1.Trapezoidal Rule
#2.Simpson's 1/3rd rule


#Trapezoidal Rule:
#Formula used : Area = h*[(y[1]+ y[n])/2 + y[2] + y[3] + y[4] + ......y[n-1]]

Function_Trapezoidal <- function(y,h)
{
  n <- length(y)
  sum <- ((y[1] + y[n])/2)
  
  for (i in 2:(n-1)) 
  {
    sum <- sum + y[i]
  }
  
  Required_Area <- (h)*sum
  
  return(Required_Area) 
}



#Simpson's 1/3rd Rule:
#Formula used : Area = = h/3[(y[0]+y[n]) + 4(y[1]+y[3]+y[5]+..+y[n-1])+2(y[2]+y[4]+y[6]+...+y[n-2])]

Function_Simpsons_one_third<- function(y,h)
{
  n <- length(y)
  if((n-1)%%2 != 0)
  {
    return(print(paste(n," is not Even. Hence,Simpson 1/3rd rule cannot be applied")))
  }
  
  sum2 <- y[1] + y[n]
  
  for (i in seq(3,n-1,2)) 
  {
    sum2 <- sum2 + 4*y[i]
  }
  
  
  for (i in seq(2,n-1,2))
  {
    sum2 <- sum2 + 2*y[i]
  }
  
  Required_Area <- ((h/3)*sum2)
  
  return(Required_Area) 
}

#For Evaluating to correct significant figures
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

# Question 5
n5 <- 6
a5 <- 0
b5 <- 0.6
h5 <- (b5 - a5)/n5
x5 <- seq(a5 , b5, h5)

Qn5_funcn  <- function(x)
{
  f <- exp(x)
  return(f)
}

Integration_Q5 <- function(x)
{
  return( exp(x)) 
}

actual_area_5 <- function(a,b)
{
  area <- Integration_Q5(b) - Integration_Q5(a)
  return(area) 
}

y5 <- Qn5_funcn(x5)

Trap_area_Q5 <- Function_Trapezoidal(y5,h5)
Simpsons_area_Q5 <- Function_Simpsons_one_third(y5,h5)
Actual_area <- actual_area_5(a5,b5)

print(" Function : e^x ")
print(paste(" Area using trapezoidal rule is :",specify_decimal(Trap_area_Q5,5)))
print(paste(" Area using Simpson 1/3 rule is :",specify_decimal(Simpsons_area_Q5,5)))
print(paste("Actual Area under the Curve : ",Actual_area))