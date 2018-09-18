#  First Name       : Tao
#  Last Name        : Chen  
#  Student ID       : 10429112
#  Creation date    : 5/1/2018

rm(list=ls())
dt<-
  read.csv("/Users/chentao/Desktop/513/csv/song.csv")
View(dt)

#total
n <- nrow(dt)
#random sequence
newdt <- dt[sample(n),]
#fetch idx
dt_idx <- sample(seq_len(n), size = round(0.7 * n))
#rate:70%training，30%test
training <- newdt[dt_idx,]
test <- newdt[-dt_idx,]
#In order
rows <- seq(nrow(dt),0.1*nrow(dt))
test <- dt[-rows,]
training <- dt[rows,]
#k=2
library(class)
predict_k1 <- knn(training[,c(-1,-2)],test[,c(-1,-2)],training[,10],k=2)
error <- (test[,10]!=predict_k1)
error_rate1 <- sum(error) / length(error)
error_rate1

#k=3
predict_k2 <- knn(training[,c(-1,-2)],test[,c(-1,-2)],training[,10],k=3)
error <- (test[,10]!=predict_k2)
error_rate2 <- sum(error) / length(error)
error_rate2

#k=5
predict_k3 <- knn(training[,c(-1,-2)],test[,c(-1,-2)],training[,10],k=5)
error <- (test[,10]!=predict_k3)
error_rate3 <- sum(error) / length(error)
error_rate3

#k=10
predict_k4 <- knn(training[,c(-1,-2)],test[,c(-1,-2)],training[,10],k=10)
error <- (test[,10]!=predict_k4)
error_rate4 <- sum(error) / length(error)
error_rate4

#k=20
predict_k5 <- knn(training[,c(-1,-2)],test[,c(-1,-2)],training[,10],k=25)
error <- (test[,10]!=predict_k5)
error_rate5 <- sum(error) / length(error)
error_rate5
plot(dt$energy,dt$loudn,col=dt$target)
