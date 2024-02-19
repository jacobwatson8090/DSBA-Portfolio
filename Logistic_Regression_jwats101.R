install.packages("e1071")
install.packages("caret")

library(e1071)
library(caret)

test_data <- read.csv("/Users/jacobwatson/Downloads/org.test.csv")
train_data <- read.csv("/Users/jacobwatson/Downloads/org.train.csv")

#Convert to factor
str(test_data)
test_data$DemGender <- as.factor(test_data$DemGender)
test_data$PromClass <- as.factor(test_data$PromClass)

train_data$DemGender <- as.factor(train_data$DemGender)
train_data$PromClass <- as.factor(train_data$PromClass)

glm.fit <- glm(TargetBuy ~ DemAffl + DemAge + DemGender + PromClass + PromSpend + PromTime,
               data = train_data)


glm.prob <- predict(glm.fit, newdata = test_data, type = "response")
glm.pred <- ifelse(glm.prob > 0.5, "1", "0")

test_data$TargetBuy <- as.factor(test_data$TargetBuy)
train_data$TargetBuy <- as.factor(train_data$TargetBuy)

#CM
table(glm.pred, test_data$TargetBuy)

confusionMatrix(as.factor(glm.pred), test_data$TargetBuy, mode = "everything", positive = "1")

Precision = 3694 / (3694 + 875)
Recall = 507 / (507 + 171)

F1 = 2 * ((0.74779 * 0.36686)/(0.74779 + 0.36686))

#Bayes
bayes.fit <- naiveBayes(TargetBuy ~ DemAffl + DemAge + DemGender + PromClass + PromSpend + PromTime,
                        data = train_data)

bayes.prob <- predict(bayes.fit, newdata = test_data, type = "raw")

bayes.pred <- predict(bayes.fit, newdata = test_data, type = "class")

table(bayes.pred, test_data$TargetBuy)

confusionMatrix(bayes.pred, test_data$TargetBuy, positive = "1")











