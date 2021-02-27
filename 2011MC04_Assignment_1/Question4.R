#Avinash Singh 2011MC04
#Question 4:

#i:Write the codes to generate A and B using rbind and cbind both the commands.

#rbind
vect1<-c(3,-2,1)
vect2<-c(-1,4,-2)
A<-rbind(vect1,vect2)
print(A)

#cbind
vec1<-c(3,-1)
vec2<-c(-2,4)
vec3<-c(1,-2)
A1<-cbind(vec1,vec2,vec3)
print(A1)

#rbind
vector1<-c(-7,4)
vector2<-c(9,5)
vector3<-c(2,-1)
B<-rbind(vector1,vector2,vector3)
print(B)

#cbind
v1<-c(-7,9,2)
v2<-c(4,5,-1)
B1<-cbind(v1,v2)
print(B1)

#ii:Calculating and printing AB
AB<-A%*%B
print(AB)

#iii:transpose matrix of AB as ABT
ABT<-t(AB)
print(ABT)

#Inverse matrix of AB as ABI
ABI<-solve(AB)
print(ABI)

#iv:
#Mean and Standard deviation of each coulumn and rows of the matrix A
ma1<-mean(A[1,])
print(ma1)
ma2<-mean(A[2,])
print(ma2)
ma3<-mean(A[,1])
print(ma3)
ma4<-mean(A[,2])
print(ma4)
ma5<-mean(A[,3])
print(ma5)

sa1<-sd(A[1,])
print(sa1)
sa2<-sd(A[2,])
print(sa2)
sa3<-sd(A[,1])
print(sa3)
sa4<-sd(A[,2])
print(sa4)
sa5<-sd(A[,3])
print(sa5)

#Mean and Standard deviation of each coulumn and rows of the matrix B
mb1<-mean(B[1,])
print(mb1)
mb2<-mean(B[2,])
print(mb2)
mb3<-mean(B[3,])
print(mb3)
mb4<-mean(B[,1])
print(mb4)
mb5<-mean(B[,2])
print(mb5)

sb1<-sd(B[1,])
print(sb1)
sb2<-sd(B[2,])
print(sb2)
sb3<-sd(B[3,])
print(sb3)
sb4<-sd(B[,1])
print(sb4)
sb5<-sd(B[,2])
print(sb5)

#mean and standard deviation of each coulumn and rows of matrix AB
m11ab<-mean(AB[1,])
print(m11ab)
m22ab<-mean(AB[2,])
print(m22ab)
m44ab<-mean(AB[,1])
print(m44ab)
m55ab<-mean(AB[,2])
print(m55ab)

s11ab<-sd(AB[1,])
print(s11ab)
s22ab<-sd(AB[2,])
print(s22ab)
s44ab<-sd(AB[,1])
print(s44ab)
s55ab<-sd(AB[,2])
print(s55ab)

#mean and standard deviation of each coulumn and rows of matrix AB transpose
m11abt<-mean(ABT[1,])
print(m11abt)
m22abt<-mean(ABT[2,])
print(m22abt)
m44abt<-mean(ABT[,1])
print(m44abt)
m55abt<-mean(ABT[,2])
print(m55abt)

s11abt<-sd(ABT[1,])
print(s11abt)
s22abt<-sd(ABT[2,])
print(s22abt)
s44abt<-sd(ABT[,1])
print(s44abt)
s55abt<-sd(ABT[,2])
print(s55abt)

#mean and standard deviation of each coulumn and rows of matrix AB inverse
m11abtinv<-mean(ABI[1,])
print(m11abtinv)
m22abtinv<-mean(ABI[2,])
print(m22abtinv)
m44abtinv<-mean(ABI[,1])
print(m44abtinv)
m55abtinv<-mean(ABI[,2])
print(m55abtinv)


s11abtinv<-sd(ABI[1,])
print(s11abtinv)
s22abtinv<-sd(ABI[2,])
print(s22abtinv)
s44abtinv<-sd(ABI[,1])
print(s44abtinv)
s55abtinv<-sd(ABI[,2])
print(s55abtinv)










