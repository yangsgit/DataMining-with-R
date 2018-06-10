rm(list = ls())
filePath <- "/Users/yangli/Desktop/breast-cancer-wisconsin.data.csv"
mydata <- read.csv(filePath, na.strings = "?")

# questoin 1.1
summary(mydata)

# question 1.2
missing <- mydata[is.na(mydata$F6),]
missing

# question 1.3
library(modeest)
F6_mfv <- mlv(mydata$F6, method = "mfv", na.rm = TRUE)
str(F6_mfv)
F6_mfv$M
mydata[is.na(mydata$F6), "F6"] <- F6_mfv$M
summary(mydata)

#quesiton 1.4
table(mydata[['Class']],mydata[['F6']])

#quesiton 1.5
pairs(mydata[c(2:5, 11)],main="Breast Cancer Graph", pch = 21, bg = c("red","green")[factor(mydata$Class)])

#question 1.6
boxplot(mydata[8:10])

#question 2
rm(list=ls())
filePath <- "/Users/yangli/Desktop/breast-cancer-wisconsin.data.csv"
mydata <- read.csv(filePath, na.strings = "?")
mydata <- na.omit(mydata)




