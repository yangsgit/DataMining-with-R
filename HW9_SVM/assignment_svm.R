rm(list=ls())
dataset<-read.csv("/Users/yangli/Desktop/IBM_Employee_Attrition_V2.csv")
library(e1071)

# making training and testing data
index <- seq(1,nrow(dataset),by=5)
test<-dataset[index,-6]
training <-dataset[-index,-6]

svmfit <- svm(Attrition~., data = training)
p_test <- predict(svmfit, test,type = "Attrition")
plot(p_test)
wrong <- (test[,"Attrition"]!=p_test)
rate <- sum(wrong) / length(wrong)
rate
