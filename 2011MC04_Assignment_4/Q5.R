#Avinash Singh 2011MC04
#Question 5
#Histogram
# Generate data for the graph. 
v<-sample.int(100, 50, replace = TRUE) #Sampling with replacement

# Create the histogram.  
hist(v, xlab = "Number", col = "orange", border = "black" ,xlim = c(0,100), ylim = c(0,20), breaks=20) 
