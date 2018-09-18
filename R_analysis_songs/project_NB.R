#  First Name       : Tao
#  Last Name        : Chen  
#  Student ID       : 10429112
#  Creation date    : 5/1/2018

rm(list=ls())
library(class) 
library(e1071)
dt<-
  read.csv("/Users/chentao/Desktop/513/csv/song.csv")
View(dt)
?prop.table
table(class=dt$target)
prop.table(table(class=dt$target))
name <- data.frame(dt$name)
for(i in 2:length(dt)){
  name <- cbind(name,as.factor(dt[,i])) 
}
data <- colnames(dt)
colnames(name) <- data

#total
n <- nrow(dt)
#fetch idx
dt_idx <- sample(seq_len(n), size = round(0.7 * n))
#rate:70%training，30%test
training <- name[dt_idx,]
test <- name[-dt_idx,]

colnames(training)[9] <- "target"
colnames(test)[9] <- "target"

nBayes_diag <- naiveBayes(target~., data = training[,-1])
nBayes_Prediction <- predict(nBayes_diag,test[,-1])

View(nBayes_Prediction)
table(actual=test[,"target"],nBayes_Prediction )
wrong <- (test[,"target"]!=nBayes_Prediction )
rate <- sum(wrong) / length(wrong)
rate

