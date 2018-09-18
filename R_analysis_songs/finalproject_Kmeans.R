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

#--------- calculating K-means ---------

kmeans.cluster <- kmeans(data, centers=2) 
tb <-table(kmeans.cluster$cluster, data_orig$target)  
tb


kmeansAccuracy <- sum(diag(tb)) / sum(tb)
#show result
cat("K-means Accuracy: ", kmeansAccuracy*100, "%\n")


require(factoextra)

fviz_cluster(kmeans.cluster,           
             data = data,              
             geom = c("point","text"), 
             frame.type = "norm")      

fviz_nbclust(data, 
             FUNcluster = kmeans,  
             method = "wss",     
             k.max = 12          
) + 
labs(title="Elbow Method for K-Means")+
geom_vline(xintercept = 4,      
             linetype = 2)         

fviz_nbclust(data, 
             FUNcluster = kmeans,   
             method = "silhouette", 
             k.max = 12             
) +
labs(title="Avg.Silhouette Method for K-Means") 