#Function to compute the minimum value of vector 'x' 
min_func <- function(x)
{
  #Let first value of the vector be minimum
  min_value <- x[1]

  for (i in 2:length(x)) 
  {
    # Comparison  
    if(min_value > x[i])
    {
      min_value <- x[i]
    }
  }
  return(min_value)}

#Function to compute the maximum value of vector 'x'
max_func <- function(x)
{
  #Let first value of the vector be maximum
  max_value <- x[1]

  for (i in 2:length(x)) 
  {
    # Comparison
    if(max_value < x[i])
    {
      max_value <- x[i]
    }
  }
  return(max_value) }

# Inputing the vector
v <- c(-4,44.7,-2,40,54,1,-3,4)

print('Input vector is :')
print(v)

print(paste('Minimum value : ',min_func(v))) 

print(paste('Maximum value : ',max_func(v)))
