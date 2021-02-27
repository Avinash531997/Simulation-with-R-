#Avinash Singh 2011MC04
#Question 4
#Box Plot
#Using the mtcars dataset
input <- mtcars[c('mpg', 'cyl')] 
print((input))
# Plot the chart. 
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders", ylab = "Miles Per Gallon", main = "Mileage Data" ,col=c("orange","white","Green")) 
