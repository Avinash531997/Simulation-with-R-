#Avinash Singh 2011MC04

#Generate a vector x using"seq(-1,3,0.01). 
#Find a vector y which contains those elements from x do not exceed 1.25. Then find the difference between the lengths of x and y.





#Defining x vector
x <- seq(from=-1, to=3, by = 0.01)
print(x)

#Defining y vector
y<-x[x<=1.25]
print(y)

#Computing length difference of x and y
Diff<-length(x)-length(y)
print(Diff)

