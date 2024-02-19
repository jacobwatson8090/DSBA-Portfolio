# install.packages 
install.packages("rpart") # recursive partitioning for classification, regression trees (CART algorithm)
install.packages("rpart.plot")
install.packages("ROCR")
install.packages("e1071") # e1071 functions for classifiers, clustering  
install.packages("rattle")
install.packages("caret")

# load package 
library(rpart)
library(rpart.plot)
# library(ROCR)
library(caret)
library(e1071)
# library(rattle)
library(tidyverse)

# import data set 
mydata <- read.csv("salary_data.csv", na.strings = c("NA", "NaN", "", "?")) 
summary(mydata)
str(mydata)

# data cleaning 
# 1. recode target variable
mydata$salary.class <- ifelse(mydata$salary.class == "<=50K", 0,1) 
str(mydata$salary.class)
mydata$salary.class <- as.factor(mydata$salary.class)

# 2. remove missing values 
mydata <- mydata[complete.cases(mydata),] # only keep the complete rows in my dataset 
# mydata <- mydata %>% drop_na()

# 3. remove variable 
mydata$fnlwgt <- NULL

# Data Partitioning--Create training and test data: 
set.seed(100)
trainIndex <- createDataPartition(mydata$salary.class, times = 1, p = 0.7, list = FALSE) # create an index for 70% of obs. by random

train_data <- mydata[trainIndex, ] # we use the index to create training data 
validation_data <- mydata[-trainIndex, ] # we use the remaining 30% as the validation data 

# Build decision trees 
tree1 <- rpart(salary.class ~ education.num + sex + race, 
               data = train_data) 

summary(tree1) # detailed summary of splits

printcp(tree1) # display the results 
rpart.plot(tree1)


# Tree 2 try to grow a larger tree 
tree2 <- rpart(salary.class ~ education.num + sex + race, data = train_data, control = rpart.control(minsplit = 2, minbucket = 1, maxdepth = 15, cp = 0.0001)) 
summary(tree2)
printcp(tree2) 
rpart.plot(tree2)
plotcp(tree2) # visualize cross-validation results 

# prune the tree2 
ptree2<- prune(tree2, cp= tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"]) # find the min error tree 
ptree2.1<- prune(tree2, cp= tree2$cptable[2,1]) # find the best prune tree
rpart.plot(ptree2) # plot the min error tree 
rpart.plot(ptree2.1) # plot best prune tree 


tree3 <- rpart(salary.class ~ education.num + sex + race+
                 capital.gain+capital.loss + age+ hours.per.week+ workclass, 
               data = train_data, 
               control = rpart.control(minsplit = 2, minbucket = 1, maxdepth = 15, cp = 0.0001))


summary(tree3)
plotcp(tree3)

ptree3<- prune(tree3, cp= tree3$cptable[which.min(tree3$cptable[,"xerror"]),"CP"]) # find the min error tree 
plotcp(ptree3)
printcp(ptree3)

ptree3.1 <- prune(tree3, cp = ptree3$cptable[8,1]) # best prune tree 
plotcp(ptree3.1)


# prediction - tree1 
predicted_values1 <- predict(tree1, validation_data, type = "prob")

head(predicted_values1)

pred1 <- as.factor(ifelse(predicted_values1[,2] > 0.5, 1,0))

confusionMatrix(pred1, # predicted class 
                validation_data$salary.class, #actual class
                positive = levels(validation_data$salary.class)[2])


# prediction - tree3 
predicted_values3 <- predict(ptree3.1, validation_data, type = "prob")
head(predicted_values3)

pred3 <- as.factor(ifelse(predicted_values3[,2] > 0.5, 1,0))

confusionMatrix(pred3, # predicted class 
                validation_data$salary.class, #actual class
                positive = levels(validation_data$salary.class)[2])


install.packages("pROC")
library(pROC)


roc1  <- roc(predictor = predicted_values1[,2], # probability =1
             response = validation_data$salary.class, # actual class
             levels = levels(validation_data$salary.class # levels of outcome 
                             ))

plot(roc1) # 0.6753
roc1$auc

roc3  <- roc(predictor = predicted_values3[,2], # probability =1
             response = validation_data$salary.class, # actual class
             levels = levels(validation_data$salary.class # levels of outcome 
             ))

plot(roc3)
roc3$auc # 0.8236  

# dev.off()

g1 <- ggroc(list(tree1 = roc1, tree3 = roc3)) + ggtitle("My ROC Curve") + xlab("1-Specificity") + ylab("Sensitivity")
g1



