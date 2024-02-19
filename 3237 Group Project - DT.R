library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(randomForest)

#load data
data <- read.csv("/Users/jacobwatson/Downloads/WineQT.csv", sep=",", header=T, strip.white = T, 
                 na.strings = c("NA","NaN","","?")) 

#Drop ID
data = subset(data, select = -c(Id))

#create decision tree targeting quality 
set.seed(100)
trainIndex <- createDataPartition(data$quality, times = 1, p = 0.7, list = FALSE)

train_data <- data[trainIndex, ]
validation_data <- data[-trainIndex, ]

#Fermentation Factors
tree1 <- rpart(quality ~ fixed.acidity + residual.sugar + volatile.acidity + chlorides,
               data = train_data)

#Summarizing 
summary(tree1)
printcp(tree1)
rpart.plot(tree1)

#Additive Affects
tree2 <- rpart(quality ~ volatile.acidity + fixed.acidity + citric.acid + free.sulfur.dioxide + 
                 total.sulfur.dioxide, data = train_data)

#Summarizing
summary(tree2)
printcp(tree2)
rpart.plot(tree2)

#Natural Qualities
tree3 <- rpart(quality ~ volatile.acidity + fixed.acidity + citric.acid + total.sulfur.dioxide +
                 alcohol + pH + density, data = train_data)

#Summarizing
summary(tree3)
printcp(tree3)
rpart.plot(tree3)

#Final Tree
tree4 <- rpart(quality ~ alcohol + density + volatile.acidity + fixed.acidity + 
                 citric.acid, data = train_data)

#Summarizing
summary(tree4)
printcp(tree4)
rpart.plot(tree4)


#Ideal Alcohol
ggplot(data, aes(x=quality)) + 
  geom_point(aes(y=alcohol), alpha=0.5, 
             position="identity")
#averaging best Alcohol percentages
alcoholmean1 <- mean(data[data$quality == 7, 'alcohol']) 

alcoholmean2 <- mean(data[data$quality == 8, 'alcohol'])
((alcoholmean1 + alcoholmean2)/2)



#Ideal Density
ggplot(data, aes(x=quality)) + 
  geom_point(aes(y=density), alpha=0.5, 
             position="identity")
#averaging best Density percentages
densitymean1 <- mean(data[data$quality == 7, 'density'])

densitymean2 <- mean(data[data$quality == 8, 'density'])
((densitymean1 + densitymean2)/2)



#Ideal Volatile Acidity
ggplot(data, aes(x=quality)) + 
  geom_point(aes(y=volatile.acidity), alpha=0.5, 
             position="identity")
#averaging best Volatile Acidity percentages
volatilemean1 <- mean(data[data$quality == 7, 'volatile.acidity'])

volatilemean2 <- mean(data[data$quality == 8, 'volatile.acidity'])
((volatilemean1 + volatilemean2)/2)



#Ideal Fixed Acidity
ggplot(data, aes(x=quality)) + 
  geom_point(aes(y=fixed.acidity), alpha=0.5, 
             position="identity")
#averaging best Fixed Acidity percentages
fixedmean1 <- mean(data[data$quality == 7, 'fixed.acidity'])

fixedmean2 <- mean(data[data$quality == 8, 'fixed.acidity'])
((fixedmean1 + fixedmean2)/2)



#Ideal Citric Acidity
ggplot(data, aes(x=quality)) + 
  geom_point(aes(y=citric.acid), alpha=0.5, 
             position="identity")
#averaging best Citric Acidity percentages
citricmean1 <- mean(data[data$quality == 7, 'citric.acid'])

citricmean2 <- mean(data[data$quality == 8, 'citric.acid'])
((citricmean1 + citricmean2)/2)



##Recode for Accuracy & CM
mydata <- data %>% 
  mutate(quality = recode(quality, `1` = 0,`2` = 0, `3` = 0,`4` = 0,`5` = 0, `5` = 0, `6` = 0,
                          `7` = 1, `8` = 1))
mydata <- mydata %>%
  mutate(across(c("quality"), factor))

#Tree Accuracy
tree.pred <- predict(tree4, validation_data, type = "class")
ConfM <- table(tree.pred, validation_data$quality)

accuracy <- sum(diag(ConfM))/sum(ConfM)
print (paste('Test accuracy is', accuracy))

#Confusion Matrix
confusionMatrix(tree.pred, data$quality)








