rm(list = ls())
library(e1071)

song <- read.csv("/Users/yangli/Desktop/song.csv")
#total
song$target <- factor(song$target)
n <- nrow(song)
#fetch idx
dt_idx <- sample(seq_len(n), size = round(0.3 * n))
#rate:70%training，30%test
training <- song[-dt_idx,-1]
test <- song[dt_idx,-1]


svmfit <- svm(target ~., data = training)
print(svmfit)
p_test <- predict(svmfit, test,type = "target")
plot(p_test)
wrong <- (test[,"target"]!=p_test)
rate <- sum(wrong) / length(wrong)
rate
