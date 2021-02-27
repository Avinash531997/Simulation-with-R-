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

# Question 2
a2 <- 0
b2 <- 5
h2 <- 1
x2 <- seq(a2 , b2, h2)

Qn2_funcn  <- function(x)
{
  f <- 1/(1+x)
  return(f)
}

Integration_Q2 <- function(x)
{
  return(log(1+x)) 
}

actual_area_2 <- function(a,b)
{
  area <- Integration_Q2(b) - Integration_Q2(a)
  return(area) 
}

y2 <- Qn2_funcn(x2)

Trap_area_Q2 <- Function_Trapezoidal(y2,h2)
Actual_area <- actual_area_2(a2,b2)
Error <- (abs(Trap_area_Q2 - Actual_area)/Actual_area)*100
print("Function : 1/(1 + x) ")
print(paste(" Area using trapezoidal rule is :",Trap_area_Q2))
print(paste("Actual Area under the Curve : ",Actual_area))
print(paste("Error : ",Error))
