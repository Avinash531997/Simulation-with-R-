#Function to compute the factorial of the element 'x' 
factorial_func <- function(x)
{
    if(x == 1 || x==0)
  {
    return(1) 
  }
    if(x>1)
    return(x*factorial_func(x-1))
}

# Input vector
input <- c(0,1,13,32)

print('Input vector is :')
print(input)


for (i in 1:length(input)) 
{
  print(paste("Factoral of ",input[i]," is  ",input[i],'! :',factorial_func(input[i]))) 
}




