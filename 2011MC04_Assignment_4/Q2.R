#Avinash Singh 2011MC04
#Question 2
#Bar Plot
Player <-c("Rohit","Subham","Pujara","Kohli","Rahane","Pant","Washington","Ashwani","Nadeem","Ishant","Bumrah")
Runs <- c(6,29,73,11,1,91,85,31,0,4,0)
barplot(Runs, xlab = "Player Name", ylab = "Runs", names.arg = Player, col=rainbow(length(Runs)),main="Player Performance")