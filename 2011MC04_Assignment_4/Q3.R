#Avinash Singh 2011MC04
#Question 3
#Bar Plot
Player <-c("Rohit","Subham","Pujara","Kohli","Rahane","Pant","Washington","Ashwani","Nadeem","Ishant","Bumrah")
Runs <- c(6,29,73,11,1,91,85,31,0,4,0)
Balls<-c(9,28,173,48,6,58,138,91,12,11,2)
Fours<-c(1,5,11,0,0,9,12,3,0,1,0)
info<-c("Runs","Balls","Fours")
colors = c("green", "orange", "brown")
mat<-cbind(Runs,Balls,Fours)
print(mat)

# Creating the matrix of the values. 
Values <- matrix(mat,nrow = 3, ncol = 11, byrow= TRUE) 
Values
# Creating the bar chart 
barplot(Values, main = "Player Performance", names.arg = Player, xlab = "Player", ylab = "Runs", col =colors, beside=TRUE) 

# Adding the legend to the chart 
legend("topright", info, cex = 0.5, fill = colors) 

# Creating the bar chart 
barplot(Values, main = "Player Performance", names.arg = Player, xlab = "Player", ylab = "Runs", col =colors)

# Adding the legend to the chart 
legend("topright", info, cex = 0.5, fill = colors)