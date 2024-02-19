library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(randomForest)

##load data
data <- read.csv("/Users/jacobwatson/Downloads/election_campaign_data.csv", sep=",", header=T, strip.white = T, 
                 na.strings = c("NA","NaN","","?")) 

##drop variables
data = subset(data, select = -c(cand_id,last_name,first_name,twitterbirth,facebookdate,
                                   facebookjan,youtubebirth))

#convert to factor
data$twitter<-as.factor(data$twitter)
data$facebook<-as.factor(data$facebook)
data$youtube<-as.factor(data$youtube)
data$cand_ici<-as.factor(data$cand_ici)
data$gen_election<-as.factor(data$gen_election)

class(data$twitter)

#remove observations with missing values
data <- data[complete.cases(data),]

#classes of gen_election
unique(data$gen_election)

#decision tree targeting gen_election
set.seed(100)
trainIndex <- createDataPartition(data$gen_election, times = 1, p = 0.7, list = FALSE)

train_data <- data[trainIndex, ]
validation_data <- data[-trainIndex, ]

tree1 <- rpart(gen_election ~ twitter + facebook + youtube, data = train_data)
#checking variable importance & plotting
summary(tree1)
printcp(tree1)
rpart.plot(tree1)

#decision tree with ttl_receipts & cand_ici
tree2 <- rpart(gen_election ~ twitter + facebook + youtube + ttl_receipts + cand_ici, data = train_data)
summary(tree2)
printcp(tree2)
rpart.plot(tree2)

plotcp(tree2)

pruned <- prune(tree2, cp = -0.05)
rpart.plot(pruned)

#min error tree
ptree2 <- prune(tree2, cp = tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"])
rpart.plot(ptree2)
printcp(ptree2)
plotcp(ptree2)

#compare decision trees

#first tree accuracy
tree.pred <- predict(tree1, validation_data, type = "class")
ConfM <- table(tree.pred, validation_data$gen_election)

accuracy <- sum(diag(ConfM))/sum(ConfM)

print (paste('Test accuracy is', accuracy))

#second tree accuracy
tree2.pred <- predict(tree2, validation_data, type = "class")
ConfM2 <- table(tree2.pred, validation_data$gen_election)

accuracy2 <- sum(diag(ConfM2))/sum(ConfM2)

print (paste('Test 2 accuracy is', accuracy2))

#first tree confusion matrix
confusionMatrix(tree.pred, validation_data$gen_election)

#second tree confusion matrix
confusionMatrix(tree2.pred, validation_data$gen_election)


