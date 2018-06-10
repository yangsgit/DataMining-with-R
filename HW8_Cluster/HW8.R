rm(list=ls())
bcw<-read.csv("/Users/yangli/Desktop/breast-cancer-wisconsin.data.csv", na.strings = "?")
bcw<- na.omit(bcw)

bcw_dist <- dist(bcw[2:10])
h_clust <- hclust(bcw_dist, method = "average")
plot(h_clust)
cluster_cut <- cutree(h_clust, k=2)

wrong <- (bcw$Class != cluster_cut)
rate <- sum(wrong) / length(wrong)
rate

km_clust <- kmeans(bcw_dist,centers = 2)
wrong <- (bcw$Class != km_clust)
rate <- sum(wrong) / length(wrong)
rate