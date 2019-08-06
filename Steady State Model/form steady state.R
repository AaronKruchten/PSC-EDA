

source("/Users/aaronkruchten/Desktop/Data Science Internship/PSC interface github copy.R")
source("/Users/aaronkruchten/Desktop/Steady State Model/steady state final (hopefully).R")
test_frame <- create_frame("/Users/aaronkruchten/Desktop/bd4d85723b4f141d96cde95c6aed3941f25ec1624ff9ecfe46bf8d0d6156e436")
lambda_mle_exp <- 1/mean(test_frame$AverageAmountOfTimeBetweenPacketLoss)
hist(test_frame$AverageAmountOfTimeBetweenPacketLoss,freq = FALSE,breaks = 50,main = "Average Amount of Time Between Packet Loss",xlab = "Seconds")
legend(20,.25,legend = "Exponential Density Function",col = "red",cex = 3/4,lty = 1)
support = c(1:40)
output <- dexp(support,rate = lambda_mle_exp)
lines(support,output,col = "red")
