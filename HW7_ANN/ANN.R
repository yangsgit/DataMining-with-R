rm(list=ls())
bcw<-
  read.csv("/Users/yangli/Desktop/breast-cancer-wisconsin.data.csv", na.strings = "?")
View(bcw)

bcw<- na.omit(bcw)
index <- seq(1, nrow(bcw), by = 5)
test <- bcw[index,]
training <- bcw[-index,]

install.packages('neuralnet')
library("neuralnet")
bcw_net <- neuralnet(Class~F1+F2+F3+F4+F5+F6+F7+F8+F9, training, hidden = 10, threshold = 0.01)
plot(bcw_net)
bcw_net_result <- compute(bcw_net, test[,c(-1,-11)])
ANN = as.numeric(bcw_net_result$net.result)
ANN_bcw <- ifelse(ANN < 2.5,2,4)
table(Actrual = test$Class, ANN_bcw)
wrong <- (test$Class != ANN_bcw)
rate <- sum(wrong)/length(wrong)
rate
