install.packages("rpart")
install.packages("randomForest")
install.packages("xgboost")
install.packages("gbm")
library(gbm)
library(rpart)
library(randomForest)
library(xgboost)
library(caret)

train_data <- read.csv("/Users/jacobwatson/Downloads/author_training.csv")
test_data <- read.csv("/Users/jacobwatson/Downloads/author_testing.csv")



#a
tree1 <- rpart(Author ~ ., data = train_data, method = "class")

tree_preds <- predict(tree1, newdata = test_data, type = "class")

tree_er <- mean(tree_preds != test_data$Author)
print(paste("Error Rate =", tree_er))


#b

train_data$Author <- as.factor(train_data$Author)

bags <- randomForest(Author ~ ., data = train_data)

bags_preds <- predict(bags, newdata = test_data)

bags_er <- mean(bags_preds != test_data$Author)
print(paste("Bagging Error Rate:", bags_er))



#c

train_data$Author <- as.factor(train_data$Author)
test_data$Author <- as.factor(test_data$Author)

boost_model <- gbm(Author ~ ., data = train_data, distribution = "multinomial", n.trees = 100, interaction.depth = 3)

boost_preds <- predict(boost_model, newdata = test_data, type = "response", n.trees = 100)

boost_preds <- colnames(boost_preds)[apply(boost_preds, 1, which.max)]

boost_error_rate <- mean(boost_preds != test_data$Author)
print(paste("Error Rate =", boost_error_rate))


# d

rf <- randomForest(Author ~ ., data = train_data, ntree = 200, mtry = sqrt(ncol(train_data) - 1))

rf_preds <- predict(rf, newdata = test_data)

rf_er <- mean(rf_preds != test_data$Author)
print(paste("Error Rate =", rf_er))


#Importance
importance(rf)




