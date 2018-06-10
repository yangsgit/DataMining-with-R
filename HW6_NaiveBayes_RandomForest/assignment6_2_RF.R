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


install.packages('randomForest')
library('randomForest')
fit <- randomForest( diagnosis~., data=training[,-1], importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
prediction <- predict(fit, test)
table(actual=test[,"diagnosis"],prediction)
table(actual=test[,"diagnosis"],prediction )
wrong<- (test[,"diagnosis"]!=prediction )
rate<-sum(wrong)/length(wrong)
rate