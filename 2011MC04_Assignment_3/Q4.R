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

# Question 4
n4 <- 4
a4 <- 1.2
b4 <- 1.6
h4 <- (b4 - a4)/n4
x4 <- seq(a4 , b4, h4)

Qn4_funcn  <- function(x)
{
  f <- x + (1/x)
  return(f)
}

Integration_Q4 <- function(x)
{
  return( (x^2)/2 + log(x)) 
}

actual_area_4 <- function(a,b)
{
  area <- Integration_Q4(b) - Integration_Q4(a)
  return(area) 
}

y4 <- Qn4_funcn(x4)

Trap_area_Q4 <- Function_Trapezoidal(y4,h4)
Simpsons_area_Q4 <- Function_Simpsons_one_third(y4,h4)
Actual_area <- actual_area_4(a4,b4)
print(" Function : x + 1/x ")
print(paste(" Area using trapezoidal rule is :",specify_decimal(Trap_area_Q4,2)))
print(paste(" Area using Simpson 1/3 rule is :",specify_decimal(Simpsons_area_Q4,2)))
print(paste("Actual Area under the Curve : ",Actual_area))

