rm(list=ls())

#--------- loading data ---------

fileName = "song.csv"
#setwd("/Users/cosoet/SIT/CS513-DataMining/FinalProject")
data_orig<- read.csv(fileName, head=TRUE, sep=",")

rmColumn <- c(1,2)
data<-data_orig[-rmColumn]
head(data)
data<- na.omit(data)
summary(data)


#--------- making training (70%) and testing (30%)  data ---------
set.seed(131) 
n <- nrow(data)
newdata <- data[sample(n),]
t_idx <- sample(seq_len(n), size = round(0.7 * n))
traindata <- newdata[t_idx,]
testdata <- newdata[ - t_idx,]


#--------- calculating Random Forest ---------

library(randomForest)
RandomRorest_Result <- randomForest(factor(target) ~ ., data = traindata, importane = T, proximity = T, do.trace = 100,mtry=2, ntree=500)
# RandomRorest_Result <- randomForest(target ~ ., data = traindata, importane = T, proximity = T, do.trace = 100)


RandomRorest_Result$err.rate

print(RandomRorest_Result)

importance(RandomRorest_Result)
varImpPlot(RandomRorest_Result)
MDSplot(RandomRorest_Result,newdata$target,palette=rep(1,5), pch=as.numeric(newdata$target))

Prediction <- predict(RandomRorest_Result, testdata)

# table result
cm <- table(x = testdata[,"target"], y = Prediction, dnn = c("origin", "prediction"))
nbAccuracy <- sum(diag(cm)) / sum(cm)
#show result
cat("Accuracy: ", nbAccuracy*100, "%\n")
cm
error<-(testdata[,"target"] !=Prediction)
error.rate <-sum(error)/length(error)
error.rate

#--------

# library(caret)
# m_rf_im<-train(factor(target)~.,data=traindata,method='rf', metric='Kappa',
#                tuneGrid=data.frame(.mtry=c(2,4,6)))
# 
# m_rf_im
