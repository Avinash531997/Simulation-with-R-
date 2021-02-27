# Taking the vector as Input
set_1 <- c(-4,44.7,-2,40,54,1,-3,4)
print('Input vector is :')
print(set_1)


#We will use Bubble Sort to Sort the elements 
# BUBBLE SORT CODE :


sort_me <- function(arr){
len=length(arr) #Finding length of the array received as argument
  for(i in 1:(len-1)){
    for(j in (i+1):len){
      if(arr[i] > arr[j]){	
	arr[c(i,j)] = arr[c(j,i)] #Swapping elements 
      }
    }
  }
  arr		#Printing the sorted Array of elements
}


#Calling the sort_me() function and passing the vector as argument.
sort_me(c(-4,44.7,-2,40,54,1,-3,4))