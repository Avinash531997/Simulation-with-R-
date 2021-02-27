#Avinash Singh 2011MC04
#Question 1

# Pie Chart
city <- c("Patna","Ratlam","Mysore","Jaunpur","Pitampura","Panchkula")
aqi <- c(276,7,92,268,412,86)
colr <- c("red", "blue", "green", "yellow", "cyan","orange")
pie(x=aqi, labels=city, radius = 0.9, main = "AQI of Indian Cities on 10 Feb 2021", col=rainbow(length(aqi)))
legend("bottomright", city, fill=rainbow(length(aqi)),cex=0.5)

#Pie-Percent
piepercent<- round(100 * aqi / sum(aqi), 1) 
pie(x=aqi, labels=piepercent, radius = 0.9, main = "AQI of Indian Cities on 10 Feb 2021", col=rainbow(length(aqi)))
legend("bottomright", city, fill=rainbow(length(aqi)),cex=0.5)

