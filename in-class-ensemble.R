# ensemble method

install.packages("mlbench")
install.packages("ipred")
install.packages("randomForest")
install.packages("fastAdaboost")
install.packages("plyr")

library(mlbench)
library(ipred)
library(randomForest)
library(fastAdaboost)
library(plyr)
library(caret)
library(rpart)
library(rpart.plot)

# data 
data(PimaIndiansDiabetes)

# data partition 
set.seed(100)
trainIndex <- createDataPartition(PimaIndiansDiabetes$diabetes, p = 0.7, list = FALSE, times = 1)

train_data <- PimaIndiansDiabetes[trainIndex, ]
valid_data <- PimaIndiansDiabetes[-trainIndex, ]

# base model- DT 
control <- trainControl(method = "cv", number = 10)

set.seed(100)
model.dt <- train(diabetes~., 
                  data = train_data, 
                  method = "rpart", 
                  trControl= control)

print(model.dt)
rpart.plot(model.dt$finalModel)

# 1. Bagging 
set.seed(100)

model.bagging <- train(diabetes ~., 
                       data = train_data, 
                       method = "treebag", 
                       nbagg = 50, 
                       trControl = control)

print(model.bagging)
plot(varImp(model.bagging))

# 2. Boosting 
set.seed(100)
model.boosting <- train(diabetes ~., 
                        data = train_data, 
                        method = "adaboost", 
                        trControl = control)
print(model.boosting)

model.boosting.tune <- train(diabetes ~., 
                        data = train_data, 
                        method = "adaboost", 
                        tuneGrid = expand.grid(nIter = c(1000, 2000), method= "Real adaboost"), 
                        trControl = control)

# random forest 
set.seed(100)
model.rf <- train(diabetes ~., 
                  data = train_data, 
                  method = "rf", 
                  ntree = 50, 
                  trControl = control)

print(model.rf)

model.rf.tune <- train(diabetes ~., 
                  data = train_data, 
                  method = "rf", 
                  tuneGrid = expand.grid(.mtry = 2:8),
                  ntree = 50, 
                  trControl = control)
print(model.rf.tune)
plot(model.rf.tune)

results <- resamples(list(dt = model.dt, bagging = model.bagging, boosting = model.boosting, rf = model.rf.tune))

summary(results)

predicted_value <- predict(model.bagging, type =  "raw", valid_data)
predicted_value

confusionMatrix(predicted_value, valid_data$diabetes, positive = levels(valid_data$diabetes)[2])

final_data <- cbind(valid_data, predicted_value)
