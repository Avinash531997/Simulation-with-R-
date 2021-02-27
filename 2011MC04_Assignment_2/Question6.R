#Function computes the gamma of the element x.
gamma_function <- function(x)
{
    if(x == 0.5 ) 
  {
    return(sqrt(pi)) 
  }
  
  if(x == 1 || x==0)
  {
    return(1) 
  }
  
  if(x>1)
  {
    return((x-1)*gamma_function(x-1))
  }
}

# Input vector
input <- c(8,2,25,3/2)


print('Input vector is :')
print(input)


for (i in 1:length(input)) 
{
  print(paste("Gamma of ",input[i]," is ",gamma_function(input[i]))) 
}

