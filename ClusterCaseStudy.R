library(dplyr)

#read data
df <- read.csv("/Users/jacobwatson/Downloads/EastWestAirlinesCluster.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 
str(df)

#Convert categorical to numerical
df2 <- df %>%
  mutate(cc1_miles = recode(cc1_miles, '1' = 2500,
                            '2' = 7500, '3' = 17500,
                            '4' = 37500, '5' = 50000))
df2 <- df2 %>%
  mutate(cc2_miles = recode(cc2_miles, '1' = 2500,
                            '2' = 7500, '3' = 17500,
                            '4' = 37500, '5' = 50000))
df2 <- df %>%
  mutate(cc3_miles = recode(cc3_miles, '1' = 2500,
                            '2' = 7500, '3' = 17500,
                            '4' = 37500, '5' = 50000))
#remove irrelevant & Scale
df.sd <- scale(df2[, c(2:11)])
summary(df.sd)

d <- dist(df.sd, method = "euclidean")

#using each method
H.single <- hclust(d, method = "single")
H.complete <- hclust(d, method = "complete")
H.average <- hclust(d, method = "average")
H.ward <- hclust(d, method = "ward.D2")

#all 4 together
par(mfrow = c(2,2))
plot(H.single)
plot(H.complete)
plot(H.average)
plot(H.ward)

par(mfrow = c(1,1))

#break into less clusters
groups <- cutree(H.ward, k = 3)
rect.hclust(H.ward, k = 3, border = "red")

cp <- aggregate(df2[, c(2:11)], list(groups), mean)
print(cp)

#k means - different method
k.means.fit <- kmeans(df.sd, 3, nstart = 25)
k.means.fit$centers










