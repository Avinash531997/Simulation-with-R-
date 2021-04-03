#2011MC04 Avinash Singh

#Given:
n<-5000

X1<-numeric(0)
X2<-numeric(0)

for(i in 1:n){
  
  while(TRUE){
    U1<-runif(1)
    U2<-runif(1)
    V1=2*U1 - 1
    V2=2*U2 - 1
    W=V1*V1 + V2*V2
    if(W>1){
      next
    }
    Y=((-2*log(W))/W)^(0.5)
    X1[i]=V1*Y
    X2[i]=V2*Y
    break
  }
}


mean(X1)
mean(X2)
var(X1)
var(X2)

Y1 = dnorm(X1, mean(X1), sd(X1))
# Plot the graph.
plot(X1, Y1)

Y2 = dnorm(X2, mean(X2), sd(X2))
# Plot the graph.
plot(X2, Y2)

