# Check working directory and set it to the directory where data files are.
getwd()
setwd("/Users/jacobwatson/Downloads")
list.files()

# read csv data into R dataframe
fitness<-read.csv(file="/Users/jacobwatson/Downloads/FITNESS.csv",header=TRUE)

# Check the dataset
head(fitness)
View(fitness)
summary(fitness)
str(fitness)

# Scatter plot, histogram, boxplots
plot(fitness$RunTime, fitness$Oxygen_Consumption)

hist(fitness$Oxygen_Consumption)
boxplot(fitness$Maximum_Pulse)
boxplot(fitness[c(3,4,5)])

# ---------------------------------------------------
# optional R code to try out
# 2x2 layout
par(mfrow=c(2,2))
boxplot(fitness$RunTime)
boxplot(fitness$Age)
par(mfrow=c(1,1))

# two-way variables plots
# A bit messy and not very useful for more than 5 variables
plot(fitness)

# Two-way scatterplots for select variables in one picture
str(fitness)
pairs(fitness[3:length(fitness)])
names(fitness)
pairs(fitness[c(6,3,10)])

# Attach and detach can simplify the expression.
attach(fitness)
plot(Performance, Oxygen_Consumption)
boxplot(Maximum_Pulse)
detach(fitness)

# Some people hate using attach because it can attach the same data frame object
# multiple tims. The following can completely detach all fitness objects.
while("fitness" %in% search()) detach(fitness)

# -----------------------------------------------------


# Run a simple linear regression - Y=Oxygen_Consumption; X1=RunTime
lm.fit <- lm(Oxygen_Consumption~RunTime, data=fitness)
lm.fit

# summary output of the regression results
summary(lm.fit)

# Analysis of Variance - to see how variance is divided
anova(lm.fit)

# Another way to get residuals sum of squares
sum(lm.fit$residuals^2)

# various plots to examine the regression results
plot(fitness$RunTime, fitness$Oxygen_Consumption)
abline(lm.fit)

# predicting the Oxygen_Consumption for RunTime=5
predict(lm.fit,data.frame(RunTime=5))



# Multiple Linear Regression with RunTime and Performance as IVs
lm.fit1 <- lm(Oxygen_Consumption~RunTime+Performance, data=fitness)
lm.fit1
summary(lm.fit1)
anova(lm.fit1)


# R-squared can be calculated from the fact 
# that it is the proportion of the variance of y explained by the model,
# R-squared = (TSS-RSS)/TSS =  1-RSS/TSS
RSS <- sum(lm.fit1$residuals^2)
TSS <- sum((fitness$Oxygen_Consumption - mean(fitness$Oxygen_Consumption))^2)
R.sqr <- 1 - (RSS/TSS)


# Residual standard error is an estimation of standard deviation of residuals
n = nrow(fitness)
rse <- sqrt(sum(lm.fit1$residuals^2)/(n-2-1))


# You can also create ANOVA table and figure out how R-squared is calculated.
anova(lm.fit1)

# Compute Adj. R-squared = 1-(n-1)(1-R-squared)/(n-p-1)
# and compare with the results output

## Qualitative variables
colnames(fitness)
str(fitness)
fitness$Gender <- as.factor(fitness$Gender)

lm.fit2 <- lm(Oxygen_Consumption ~ Age + Gender,
             data=fitness)

summary(lm.fit2)



# Running stepwise selection of independent variables
# Gender is a qualitative predictor.
# To work in regression as an indicator variable, it should be made a factor
# Indicator variables are automatically created in R if the type is a factor
fitness$Gender<-as.factor(fitness$Gender)

# Forward selection
step.forward <- step(lm(Oxygen_Consumption~1, data=fitness),
                     scope ~ (RunTime + Age + Gender + Weight +
                                Run_Pulse + Rest_Pulse + Performance),
                     direction = "forward", trace=TRUE)

summary(step.forward)

# Backward elimination
step.backward <- step(lm(Oxygen_Consumption~RunTime + Age + Gender + Weight +
                           Run_Pulse + Rest_Pulse + Performance, data=fitness),
                      direction = "backward", trace=TRUE)

summary(step.backward)


# Stepwise selection - combines forward & backward
step.both <- step(lm(Oxygen_Consumption~1, data=fitness),
                     scope ~ (RunTime + Age + Gender + Weight +
                                Run_Pulse + Rest_Pulse + Performance),
                     direction = "both", trace=TRUE)

summary(step.both)


## Types of errors
## Residual sum of squares (error sum of squares)
## Total sum of squares
## Mean Square Error (MSE) - considers degrees of freedom
## Residual Standard Error (from regression output)

anova(lm.fit1)
fitness$predict <- predict(lm.fit1, fitness)
rss <- sum((fitness$Oxygen_Consumption - fitness$predict)^2)
tss <- sum((fitness$Oxygen_Consumption - mean(fitness$Oxygen_Consumption))^2)


## Multicollinearity
## install and load "car" package
install.packages("car")
library(car)

vif(lm.fit1)
# vif values should be less than 10 to reduce multicollinearity concerns


# Create training and test data
# following method uses random sampling to create train and test data
n = nrow(mydata)

trainIndex <- sample(1:n, 
                     size = round(0.7*n),
                     replace = FALSE)

train_data <- mydata[trainIndex, ] # we use the index to create training data 
test_data <- mydata[-trainIndex, ] # we use the remaining 30% as the test data 


