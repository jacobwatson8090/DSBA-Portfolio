
## Evaluating classifiers - using TITANIC_FORMATTED.csv data

titanic <- read.csv("TITANIC_FORMATTED.csv", stringsAsFactors=TRUE)

# We will remove missing values, convert categorical variable to factors,
# and run the logistic regression.
titanic <- na.omit(titanic)
titanic$Gender <- as.factor(titanic$Gender)
titanic$Class <- as.factor(titanic$Class)
titanic$Survival <- as.factor(titanic$Survival)

# Let's randomly pick 70% based on the row number and split the data
n = nrow(titanic)
trainrows <- sample(n, as.integer(0.7*n), replace = FALSE)
titan_train <- titanic[trainrows, ]
titan_test <- titanic[-trainrows, ]

glm.fit<-glm(Survival~Age+Gender+Class,family=binomial,data=titan_train)

summary(glm.fit)

# Let us predict test data using the logistic regression model
glm.prob <- predict(glm.fit, newdata=titan_test, type="response")
glm.pred <- ifelse(glm.prob > 0.5, "Survived", "Died")


# naiveBayes is a function in e1071
install.packages("e1071")
library(e1071)

bayes.fit <- naiveBayes(Survival~Age+Gender+Class,data=titan_train)

bayes.fit

# to predict probabilities, use type="raw"
bayes.prob <- predict(bayes.fit, newdata = titan_test, type = "raw")

# to predict class, use type="class" (default p cutoff is 0.5)
bayes.pred <- predict(bayes.fit, newdata = titan_test, type = "class")

head(bayes.prob)
head(bayes.pred)



# Now let us construct the confusion matrices for each.
#  For logistic regression output
table(glm.pred, titan_test$Survival)

# for Naive Bayes output
table(bayes.pred,titan_test$Survival)


# Calculate different evaluation metrics by hand first.
# Accuracy, Error Rate, Precision (Positive Predicted Value)
# Recall (Sensitivity or True Positive Rate)
# Specificity (True Negative Rate) 
# F1 score = 2 * (Precision * Recall) / (Precision + Recall) 
# = Harmonic Mean of Precision and Recall 
# Type I error = (1 - Specificity)
# Type II error = (1 - Sensitivity)


# There is an R package caret that calculates these metrics
install.packages("caret")
library(caret)

# confusionMatrix() function is available in caret package
confusionMatrix(as.factor(glm.pred), titan_test$Survival, positive = "Survived")
confusionMatrix(bayes.pred, titan_test$Survival, positive = "Survived")

# Details on each calculation are available in the documentation.
?confusionMatrix()
