library(cluster)
library(caret)
library(dendextend)
library(factoextra)
library(purrr)


c_data <- read.csv("A:/DATA_SETS/Cereals.csv")
sum(is.na(c_data))

c_data <- na.omit(c_data)
c_data <- c_data[,4:16]
c_data <- scale(c_data, center = T, scale = T)
summary(c_data)
head(c_data)
set.seed(123)

# Dissimilarity matrix
euclidean_dist <- dist(c_data, method = "euclidean")


method <- c( "average", "single", "complete", "ward")
names(method) <- c( "average", "single", "complete", "ward")
ac_values <- function(x) {
  
  agnes(euclidean_dist, method = x)$ac
}

map_dbl(method, ac_values)

#The agglomerative coefficient obtained by Ward's method is the largest. 
#Let's take a peek at the dendogram.

hc_ward <- agnes(euclidean_dist, method = "ward")
pltree(hc_ward, cex = 0.5, hang = -1, main = "Dendrogram of agnes for ward")

#install.packages("NbClust")
library(NbClust)
num_of_clust = NbClust(c_data, distance = "euclidean", min.nc = 5, max.nc = 10, method = "ward.D",index = 'dunn')
num_of_clust$Best.nc

#After checking NbClust value for best number of clusters, the best fits is with K=7
rect.hclust(hc_ward, k = 7, border = 2:10)


clust_comp <- cutree(hc_ward, k = 7)
temp3 <- cbind(as.data.frame(cbind(c_data,clust_comp)))

#Q3

c_data <- read.csv("A:/DATA_SETS/Cereals.csv")
sum(is.na(c_data))
c_data <- na.omit(c_data)
c_data <- c_data[,4:16]
# Creating Partitions for into two data  
c_partition_A <- c_data[1:37,]
c_partition_B <- c_data[38:74,]
c_partition_A <- scale(c_partition_A, center = T, scale = T)
c_partition_B <- scale(c_partition_B, center = T, scale = T)

euclidean_dist_partition_A <- dist(c_partition_A, method = "euclidean")

names(method) <- c( "average", "single", "complete", "ward")
ac_values1 <- function(x) {
  agnes(euclidean_dist_partition_A, method = x)$ac
}
map_dbl(method, ac_values1)

#The agglomerative coefficient obtained by Ward's method is the largest. 
#Let's take a peek at the dendogram.
set.seed(123)
hc_ward_partition_A <- agnes(euclidean_dist_partition_A, method = "ward")
pltree(hc_ward_partition_A, cex = 0.5, hang = -1, main = "Dendrogram of agnes for ward")

clust_comp_partition_A <- cutree(hc_ward_partition_A, k = 7)

result<-as.data.frame(cbind(c_partition_A,clust_comp_partition_A))

#result[result$clust_comp_partition_A==1,]
#center1<-colMeans(result[result$clust_comp_partition_A==1,])
klust <- 1:7
for (i in klust) {
  assign(paste0("center_",i), colMeans(result[result$clust_comp_partition_A==i,]))
}

centroids <- rbind(center_1,center_2,center_3,center_4,center_5,center_6,center_7
                   )

combined <- as.data.frame(rbind(centroids[,-14], c_partition_B))
temp1<-get_dist(combined)
temp2<-as.matrix(temp1)
data1<-data.frame(data=seq(1,nrow(c_partition_B),1),clusters=rep(0,nrow(c_partition_B)))
for(i in 1:nrow(c_partition_B))
{
  data1[i,2]<-which.min(temp2[i+7,1:7])
}
data1
cbind(temp3$clust_comp[38:74],data1$clusters)
table(temp3$clust_comp[38:74]==data1$clusters)

#We get 17 FALSE and 20 TRUE, indicating that the model is only partly stable.

#4

Nutri_cereal <- na.omit(read.csv("A:/DATA_SETS/Cereals.csv"))
summary(Nutri_cereal)
Nutri_cereal<- cbind(Nutri_cereal,clust_comp)


for (i in 1:7){
  assign(paste0("Cluster",i), mean(Nutri_cereal[Nutri_cereal$clust_comp==i,"rating"]))

}

a<-cbind(Cluster1,Cluster2,Cluster3,Cluster4,Cluster5,Cluster6,Cluster7)
a
paste("clearly cluster 1 has maximum rating", max(a)," hence we'll choose it")
