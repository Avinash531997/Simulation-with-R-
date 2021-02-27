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

# Question 1

n1 <- 10
a1 <- 0
b1 <- 1
h1 <- (b1 - a1)/n1
x1 <- seq(a1 , b1, h1)
print("Values of x :")
x1


#Evaluating y=f(x) for all values of x in [a,b] with given h.
#The function below will basically compute y[0],y[1],y[2],....y[n]
Qn1_funcn  <- function(x)
{
  f <- 4*x- 3*(x^2)
  return(f)
}

#Function after integrating f(x)
Integration_Q1 <- function(x)
{
  return(2*(x^2) - (x^3)) 
}

actual_area_1 <- function(a,b)
{
  area <- Integration_Q1(b) - Integration_Q1(a)
  return(area) 
}


#Generating all values of y for corresponding values of x in interval [a,b] with given h.
y1 <- Qn1_funcn(x1)

Trap_area_Q1 <- Function_Trapezoidal(y1,h1)
Simpsons_area_Q1 <- Function_Simpsons_one_third(y1,h1)
Actual_area <- actual_area_1(a1,b1)
Error_T <- (abs(Trap_area_Q1 - Actual_area)/Actual_area)*100
Error_S <- (abs(Simpsons_area_Q1 - Actual_area)/Actual_area)*100
print(" Function :   4x - 3(x^2) ")
print(paste(" Area using Trapezoidal rule is :",Trap_area_Q1))
print(paste(" Area using Simpson 1/3 rule is :",Simpsons_area_Q1))
print(paste("Actual Area under the Curve : ",Actual_area))
print(paste("Error T : ",Error_T))
print(paste("Error S: ",Error_S))
