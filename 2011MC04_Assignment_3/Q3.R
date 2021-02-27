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

# Question 3
n3 <- 6
a3 <- 0
b3 <- 5
h3 <- (b3 - a3)/n3
x3 <- seq(a3 , b3, h3)

Qn3_funcn  <- function(x)
{
  f <- x/(1+x)
  return(f)
}

Integration_Q3 <- function(x)
{
  return(x - log(1+x)) 
}

actual_area_3 <- function(a,b)
{
  area <- Integration_Q3(b) - Integration_Q3(a)
  return(area) 
}

y3 <- Qn3_funcn(x3)

Trap_area_Q3 <- Function_Trapezoidal(y3,h3)
Simpsons_area_Q1 <- Function_Simpsons_one_third(y3,h3)
Actual_area <- actual_area_3(a3,b3)
print("Function : x/(1+x)")
print(paste(" Area using trapezoidal rule is :",specify_decimal(Function_Trapezoidal(y3,h3), 3)))
print(paste(" Area using Simpson 1/3 rule is :",specify_decimal(Function_Simpsons_one_third(y3,h3),3)))
print(paste("Actual Area under the Curve : ",Actual_area))
