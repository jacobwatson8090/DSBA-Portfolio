#CLUSTER ANALYSIS
install.packages("cluster")
install.packages("clustertend")
install.packages("dbscan")

library(cluster)
library(clustertend)
library(dbscan)

data <- read.csv("/Users/jacobwatson/Downloads/dow_jones_data.csv", sep=",", header=T, 
              strip.white = T, na.strings = c("NA","NaN","","?")) 

df <- scale(data[-1])
# or df <- scale(data, [, c(2:14)])
summary(df)

# k-means 
k.means.fit <- kmeans(df, 3, nstart = 25)

attributes(k.means.fit)

k.means.fit$cluster
k.means.fit$centers
k.means.fit$size

#optimizing K
withinssplot <- function(data, nc = 30, seed = 100) {
  wss <- (nrow(data)-1) * sum(apply(data, 2, var))
  for (i in 2:nc){
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
}

withinssplot(df, nc = 15)

k.means.fit$centers
k.means.fit$size

clusplot(df, k.means.fit$cluster, main = "2D representation of the cluster solution", 
         color = TRUE, shade = TRUE, labels = 2, lines = 0)

#HIERARCHICAL CLUSTERING

#Generate Distance Matrix
d <- dist(df, method = "euclidean")
H.single <- hclust(d, method = "single")
plot(H.single)

H.complete <- hclust(d, method = "complete")

H.average <- hclust(d, method = "average")

H.ward <- hclust(d, method = "ward.D2")

par(mfrow = c(2,2))
plot(H.single)
plot(H.complete)
plot(H.average)
plot(H.ward)

par(mfrow = c(1,1))

group <- cutree(H.ward, k = 3)
group

rect.hclust(H.ward, k = 4, border = "red")

install.packages("dbscan")
library(dbscan)

kNNdistplot(df, k = 4)
abline(h = 3, col = "red")

db <- dbscan(df, eps = 3, minPts = 4)
db$cluster

clusplot(df, db$cluster, main = "2D representation of the cluster solution", 
         color = TRUE, shade = TRUE, labels = 2, lines = 0)
hullplot(df, db)

plot(silhouette(k.means.fit$cluster, d)) #0.25
plot(silhouette(group, d)) #0.29
plot(silhouette(db$cluster), d) #0.39

install.packages("clustertend")
library(clustertend)
install.packages("hopkins")
library(hopkins)

hopkins <- 1-hopkins(df, n = nrow(df)-1)$H










