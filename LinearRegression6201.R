install.packages("mlbench")
install.packages("ipred")
install.packages("randomForest")
install.packages("fastAdaboost")
install.packages("plyr")
install.packages("dplyr")
install.packages("magrittr")
install.packages("pROC")
install.packages("lattice")
install.packages("car")


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
library(car)

#Read data
cp <- read.csv("/Users/jacobwatson/Downloads/carprices.csv", sep=",", header=T, 
                   strip.white = T, na.strings = c("NA","NaN","","?")) 
cp <- cp %>%
  mutate_if(is.character, as.factor)

#Boxplot - Question 1
boxplot(cp$price)

#Histogram - Question 2
hist(cp$price)

#Log transform - Question 3
lprice = log(cp$price)

hist(lprice)
boxplot(lprice)

#Scatterplots - Question 4
plot(x = lprice, y = cp$horsepower, main = "lprice vs Horsepower", xlab = "lprice", ylab = "Horsepower")
plot(x = lprice, y = cp$citympg, main = "lprice vs CityMPG", xlab = "lprice", ylab = "CityMPG")

#MLR - Question 5
n = nrow(cp)

trainIndex <- sample(1:n, 
                     size = round(0.7*n),
                     replace = FALSE)

train_data <- cp[trainIndex, ]
test_data <- cp[-trainIndex, ]

cp$fueltype <- factor(cp$fueltype)

lm.fit1 <- lm(lprice ~ cp$fueltype + cp$enginesize + cp$stroke + cp$peakrpm + cp$citympg + cp$highwaympg 
              + cp$carwidth + cp$aspiration + cp$boreratio, data=cp)
summary(lm.fit1)

#Drop Highest VIF - Question 6
install.packages("car")
library(car)

vif(lm.fit1)

lm.fit2 <- lm(lprice ~ cp$fueltype + cp$enginesize + cp$stroke + cp$peakrpm + cp$highwaympg 
              + cp$carwidth + cp$aspiration + cp$boreratio, data=cp)

vif(lm.fit2)

lm.fit3 <- lm(lprice ~ cp$fueltype + cp$enginesize + cp$stroke + cp$peakrpm + cp$carwidth 
              + cp$aspiration + cp$boreratio, data=cp)

vif(lm.fit3)

lm.fit4 <- lm(lprice ~ cp$fueltype + cp$stroke + cp$peakrpm + cp$carwidth 
              + cp$aspiration + cp$boreratio, data=cp)

vif(lm.fit4)



#Question 7
summary(lm.fit4)
anova(lm.fit4)

#Question 9
RSS <- sum(lm.fit4$residuals^2)
TSS <- sum((lprice - mean(lprice))^2)
R.sqr <- 1 - (RSS/TSS)
print(R.sqr)

#Question 12
predict(lm.fit4, train_data)

predict(lm.fit4, test_data)

#MSE
test <- cp[1:165,]
train <- cp[166:205,]

#train data
lm.fit5 <- lm(lprice ~ cp$fueltype + cp$stroke + cp$peakrpm + cp$carwidth + cp$aspiration + cp$boreratio,
              data = train)

anova(lm.fit5)

predicted <- predict(lm.fit5, train)


RSS_training <- sum((log(train$price)- predicted)^2)
MSE_training <- RSS_training / 197


#test data
lm.fit6 <- lm(lprice ~ cp$fueltype + cp$stroke + cp$peakrpm + cp$carwidth + cp$aspiration + 
                cp$boreratio, data = test)

anova(lm.fit6)

predicted2 <- predict(lm.fit6, test)
RSS_test <- sum((log(test$price)- predicted2)^2)
MSE_test<- RSS_test / 197
print(MSE_training)
print(MSE_test)




















