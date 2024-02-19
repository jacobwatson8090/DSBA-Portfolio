library(data.table)
library(cluster)
library(clustertend)
library(dbscan)
library(hopkins)

#read data
data <- fread("/Users/jacobwatson/Downloads/hospital_ortho.csv", sep=",", header=T, strip.white = T, 
              na.strings = c("NA","NaN","","?"))
#narrow data
nc_data <- data[(data$state == "NC") | (data$state == "SC") | 
                  (data$state == "VA") | (data$state == "GA") | 
                  (data$state == "TN")]

#Scale data
nc_data<-nc_data[,c(5:13,17:19)]
df <- scale(nc_data)

#k-means clustering
kmeans.fit <- kmeans(df,3)
kmeans.fit$cluster
kmeans.fit$size
hopkins(df, m = nrow(df)-1)

#Plot Within Groups SSE
withinssplot <- function(data, nc=15, seed=1234){  
  wss <- (nrow(data)-1)*sum(apply(data,2,var))  
  for (i in 2:nc){    
    set.seed(seed)    
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",       
       ylab="Within groups sum of squares")}

withinssplot(df, nc=10)

#k-means clustering
kmeans.fit<-kmeans(df,4)
kmeans.fit$size

#representing clusters
clusplot(df, kmeans.fit$cluster, main='2D representation of the Cluster solution', 
         color=TRUE, shade=TRUE,labels=2, lines=0)

#different clustering methods
d <- dist(df, method = "euclidean")

H.single <- hclust(d, method="single")
H.complete <- hclust(d, method="complete")
H.average <- hclust(d, method="average")
H.ward <- hclust(d, method="ward.D2")

par(mfrow=c(2,2))
plot(H.single)
plot(H.complete)
plot(H.average)
plot(H.ward)

par(mfrow=c(1,1))
#show clusters
groups <- cutree(H.ward, k=3)
clusplot(df, groups, main='2D representation of the Cluster solution',         
         color=TRUE, shade=TRUE, labels=2, lines=0)

#red lines
plot(H.ward)
rect.hclust(H.ward, k=3, border="red")

#Min Pts
pca <- prcomp(df, center = TRUE, scale. = TRUE)
print(pca)
plot(pca, type = "l")
summary(pca)

#EPS
kNNdistplot(df, k =4)
abline(h=3.3, col="red")

db <- dbscan(df, eps=3.3, minPts=4)
db

#DB Scan Clustering
clusplot(df, db$cluster, main='2D representation of the Cluster solution',         
         color=TRUE, shade=TRUE,         
         labels=2, lines=0)

#silhouettes
plot(silhouette(kmeans.fit$cluster, d))
plot(silhouette(groups, d))
plot(silhouette(db$cluster, d))










