rm(list=ls())
install.packages('e1071', dependencies = TRUE)
library(class) 
library(e1071)

bcw<-
  read.csv("/Users/yangli/Desktop/breast-cancer-wisconsin.data.csv")
View(bcw)
?prop.table
table(class=bcw$Class)
prop.table(table(Class=bcw$Class))

sample <- data.frame(bcw$Sample)
for(i in 2:length(bcw)){
  sample <- cbind(sample,as.factor(bcw[,i])) 
}
data <- colnames(bcw)
colnames(sample) <- data
index <- seq (1,nrow(sample),by=5)
test <- sample[index,]
training <- sample[-index,]

colnames(training)[11] <- "diagnosis"
colnames(test)[11] <- "diagnosis"

nBayes_diag <- naiveBayes( diagnosis~., data =training[,-1])
nBayes_Prediction <- predict( nBayes_diag, test)
table(actual=test[,"diagnosis"],nBayes_Prediction )
wrong <- (test[,"diagnosis"]!=nBayes_Prediction )
rate <- sum(wrong) / length(wrong)
rate