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

#--------- Normalize data ---------

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
dataSet_norm <- as.data.frame(lapply(data[1:7], normalize))

data[1:7] <- dataSet_norm



#--------- making training (70%) and testing (30%)  data ---------
set.seed(131)         
n <- nrow(data)
newdata <- data[sample(n),]
t_idx <- sample(seq_len(n), size = round(0.7 * n))
traindata <- newdata[t_idx,]
testdata <- newdata[ - t_idx,]

#--------- calculating ANN ---------

library(neuralnet)
n <- names(traindata)
f <- as.formula(paste("target ~", paste(n[!n %in% "target"], collapse = " + ")))
ann <- neuralnet(formula = f,data=traindata,hidden=c(5,3),linear.output=T,stepmax=1e6, threshold = 0.01)

plot(ann)


result <-compute(ann,testdata[,-8])
summary(result$net.result)

pred.result <- round(result$net.result)

#--------- result ---------
tb <-table(pred.result, testdata$target)  
tb

ANNAccuracy <- sum(diag(tb)) / sum(tb)
#show result
cat("ANN Accuracy: ", ANNAccuracy*100, "%\n")
tb
error<-(testdata[,"target"] !=pred.result)
error.rate <-sum(error)/length(error)
error.rate

#-------


