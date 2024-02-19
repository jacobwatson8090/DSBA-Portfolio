install.packages("mlbench")
install.packages("ipred")
install.packages("randomForest")
install.packages("fastAdaboost")
install.packages("plyr")
install.packages("dplyr")
install.packages("magrittr")
install.packages("pROC")
install.packages("lattice")


library(mlbench)
library(ipred)
library(fastAdaboost)
library(plyr)
library(dplyr)
library(magrittr)
library(pROC)
library(lattice)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(randomForest)

##load data
mydata <- read.csv("/Users/jacobwatson/Downloads/WineQT.csv", sep=",", header=T, 
                 strip.white = T, na.strings = c("NA","NaN","","?")) 

##drop variables
mydata = subset(mydata, select = -c(Id))

##Recode
mydata <- mydata %>% 
  mutate(quality = recode(quality, `1` = 0,`2` = 0, `3` = 0,`4` = 0,`5` = 0, `5` = 0, `6` = 0,
                          `7` = 1, `8` = 1))
mydata <- mydata %>%
  mutate(across(c("quality"), factor))

#Data partition
set.seed(100)
trainIndex <- createDataPartition(mydata$quality, times = 1, p = 0.7, list = FALSE)

train_data <- mydata[trainIndex, ]
validation_data <- mydata[-trainIndex, ]

#create control
control <- trainControl(method = "cv", number = 10)
train

#Create decision tree
set.seed(100)
model.dt <- train(quality ~., 
                  data = train_data, 
                  method = "rpart", 
                  trControl = control)

print(model.dt)     

#Create random forest
set.seed(100)
model.rf <- train(quality ~., 
                  data = train_data, 
                  method = "rf", 
                  tuneGrid = expand.grid(.mtry = 2),
                  ntree = 5, trControl = control)

print(model.rf)

#change mtry
set.seed(100)
model.rf.tune <- train(quality ~., 
                       data = train_data, 
                       method = "rf", 
                       tuneGrid = expand.grid(.mtry = 7),
                       ntree = 5, trControl = control)

print(model.rf.tune)

#10 ntree
set.seed(100)
model.rf.nt <- train(quality ~., 
                       data = train_data, 
                       method = "rf", 
                       tuneGrid = expand.grid(.mtry = 7),
                       ntree = 10, trControl = control)

print(model.rf.nt)

#50 ntree
set.seed(100)
model.rf.fty <- train(quality ~., 
                     data = train_data, 
                     method = "rf", 
                     tuneGrid = expand.grid(.mtry = 7),
                     ntree = 50, trControl = control)

print(model.rf.fty)

#confusion matrix
predicted_value <- predict(model.rf.fty, type = "raw", validation_data)
print(predicted_value)

confusio

threshold = 0.5
confusionMatrix(predicted_value, validation_data$quality,
                positive = levels(validation_data$quality)[2])

#ROC Curve
predicted_value <- as.numeric(predicted_value)
roc1 <- roc(response = validation_data$quality, predictor = predicted_value)

roc1  <- roc(predictor = predicted_value,
             response = validation_data$quality, 
             levels = levels(validation_data$quality))
plot(roc1)

#Variable Importance
varImp(model.rf.fty)

