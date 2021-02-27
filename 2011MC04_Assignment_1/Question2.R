#Avinash Singh 2011MC04
#Question 2:
#Write a R code to generate a vector of length 100, in which 1st 10 elements
#are 50, next 30 elements are the combinations of 1 and 2, next 50 elements are from -1 to
#3, next 8 elements are 2 and last 2 elements are the last two digits of your roll number.




#50 50 50 ...10 times
x1<-rep(50,times=10)   

#1 2 1 2 ....15 times(total 30 elements)   
x2<-rep(1:2,times=30/2)  

#-1 0 1 2 3 ....50 total 
x3<-rep(-1:3,times=50/5)

#2 2 2...8times
x4<-rep(2,times=8) 

#last 2 digits of Rollno      
x5<-c(0,4)

#merging all above vectors into one vector of length 100
final<-c(x1,x2,x3,x4,x5)   
print(final)
