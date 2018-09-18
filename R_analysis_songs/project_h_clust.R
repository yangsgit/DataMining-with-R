#  First Name       : Tao
#  Last Name        : Chen 
#  Student ID       : 10429112
#  Creation date    : 5/1/2018

#load the csv file
rm(list=ls())
dt<-
  read.csv("/Users/chentao/Desktop/513/csv/song.csv")
View(dt)
summary(dt)

dt_dist <- dist(dt[3:9]) 
result <- hclust(dt_dist, method = "average")
plot(result)
cut_cluster <- cutree(result, k=3)
table <- table(cut_cluster, dt$target)
wrong <- (dt$target!=cut_cluster)
err_rate <- sum(wrong)/length(wrong)
err_rate

dt_dist <- dist(dt[3:9]) 
result <- hclust(dt_dist, method = "single")
plot(result)
cut_cluster <- cutree(result, k=3)
table <- table(cut_cluster, dt$target)
wrong <- (dt$target!=cut_cluster)
err_rate <- sum(wrong)/length(wrong)
err_rate

dt_dist <- dist(dt[3:9]) 
result <- hclust(dt_dist, method = "complete")
plot(result)
cut_cluster <- cutree(result, k=3)
table <- table(cut_cluster, dt$target)
wrong <- (dt$target!=cut_cluster)
err_rate <- sum(wrong)/length(wrong)
err_rate

dt_dist <- dist(dt[3:9]) 
result <- hclust(dt_dist, method = "centroid")
plot(result)
cut_cluster <- cutree(result, k=3)
table <- table(cut_cluster, dt$target)
wrong <- (dt$target!=cut_cluster)
err_rate <- sum(wrong)/length(wrong)
err_rate
