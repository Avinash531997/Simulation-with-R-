var = readline(prompt = "Enter any number : "); 
#Converting the inputted value to an integer 
var = as.integer(var);

check_prime <- function(var) 
{
	if(var<=1)
	return(FALSE)
	
	else if(var==2)
	return(TRUE)

      for( i in 2:(var-1)) 
	{
      	if(var %% i == 0)
			{     
			return(FALSE)
			}
	}
	return(TRUE)
}


if(check_prime(var)==TRUE){
print("Prime")
}else{ 
print("Composite")}


