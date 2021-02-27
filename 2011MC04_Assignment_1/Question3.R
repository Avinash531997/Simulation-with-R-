#Avinash Singh 2011MC04
#Question 3:

#Initializing Vectors
vect1<-c(1.5,-1,-1,3)

#Initializing matrix
mat1 <- matrix(vect1, nrow=2, ncol=2)
x<-c(4,7)
y<-c(1,0)

#Picking column wise elements from ecah matrix and vectors using coulumn bind 
mat2<-cbind(mat1,x,y)   
print(mat2)