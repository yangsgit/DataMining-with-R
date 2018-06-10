#Question 1
rm(list = ls())
train <- read.csv("/Users/yangli/Desktop/Book2.csv")
train_iris <- train[,c(1,2,3,4)]
test <- read.csv("/Users/yangli/Desktop/Book1.csv")
test_iris <- test[,c(1,2,3,4)]
target_iris <- train[,5]

library(class)
predict_k2 <- knn(train = train_iris, test = test_iris, cl = target_iris, k = 2)
#the error rate is 1/3
          #setosa versicolor virginica
#setosa         1          0         0
#virginica      0          1         1

# Question 2
rm(list = ls())
library(class)
filePath <- "/Users/yangli/Desktop/breast-cancer-wisconsin.data.csv"
bcw <- read.csv(filePath,na.strings = "?")
bcw_clean <- na.omit(bcw)

test <- bcw_clean[seq(1, nrow(bcw_clean), by = 5),]
train <- bcw_clean[-seq(1, nrow(bcw_clean), by = 5),]


#k = 1 prediction 2 : 75  4 : 62   there are two mistakes in each target 
predict_k1 <- knn(train[,c(-1,-11)], test[,c(-1,-11)], train[,11], k = 1)
wrong <- test[,11] != predict_k1
rate1 <- sum(wrong)/length(wrong)
rate1
# rate = 0.05109489


predict_k2 <- knn(train[,c(-1,-11)], test[,c(-1,-11)], train[,11], k = 2)
wrong <- test[,11] != predict_k2
rate2 <- sum(wrong)/length(wrong)
rate2
#rate = 0.03649635

predict_k5 <- knn(train[,c(-1,-11)], test[,c(-1,-11)], train[,11], k = 5)
wrong <- test[,11] != predict_k5
rate5 <- sum(wrong)/length(wrong)
rate5
#rate = 0.04379562

predict_k10 <- knn(train[,c(-1,-11)], test[,c(-1,-11)], train[,11], k = 10)
wrong <- test[,11] != predict_k10
rate10 <- sum(wrong)/length(wrong)
rate10
#rate = 0.04379562
#Conclusion : when k = 2 we get the best prediction,the wrong rate is lowest, if k is too small or too big, both can improve wrong rate



