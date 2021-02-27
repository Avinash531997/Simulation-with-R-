#Avinash Singh 2011MC04

#Question 1:
#Defining x and y

x<-c(4,-5,2)
y<-c(6,1,-3)

#Display length of x and y vector
length(x)->valx
print(valx)
length(y)->valy
print(valy)

#Addition and Subtraction of vectors x and y.

x+y->add
x-y->sub
print(add)
print(sub)

#Summation over x and y
print(sum(x))
print(sum(y))

#Covariance of x and y using function and formula
print(cov(x,y))

#Using the given formula
a<-x-mean(x)
b<-y-mean(y)
c<-a*b
d<-sum(c)
f<-d/(valx-1)
print(f)