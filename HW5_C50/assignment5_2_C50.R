rm(list = ls()) 
bcw <- read.csv("/Users/yangli/Desktop/breast-cancer-wisconsin.data.csv")
View(bcw)


library('C50')

bcw$F1 <- as.factor(bcw$F1)
bcw$F2 <- as.factor(bcw$F2)
bcw$F3 <- as.factor(bcw$F3)
bcw$F4 <- as.factor(bcw$F4)
bcw$F5 <- as.factor(bcw$F5)
bcw$F6 <- as.factor(bcw$F6)
bcw$F7 <- as.factor(bcw$F7)
bcw$F8 <- as.factor(bcw$F8)
bcw$F9 <- as.factor(bcw$F9)
bcw$Class <- as.factor(bcw$Class)


index<-sort(sample(nrow(bcw),round(nrow(bcw))))
training<-bcw[-index,]
test<-bcw[index,]

Class <- C5.0( Class~., data = bcw)
predict <- predict(Class ,test , type = "class" )
table(actual = test[,11],C50 = predict)
wrong <- (test[,11] != predict)
rate <- sum(wrong) / length(test[,11])
rate