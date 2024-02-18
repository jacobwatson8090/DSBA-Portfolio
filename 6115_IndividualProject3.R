# Load required library
library(e1071)
library(rpart)
install.packages("naivebayes")
library(naivebayes)

# Create the dataset
data <- read.csv("/Users/jacobwatson/Downloads/6115IP.csv")

# Impute "Thunder" variable
data$Thunder <- as.factor(ifelse(data$Thunder == "Sunny", 1, 0))
data$Hailstorm <- as.factor(ifelse(data$Hailstorm == "Hot", 1, 0))
data$Homework <- as.factor(ifelse(data$Homework == "High", 1, 0))
data$Tsunami <- as.factor(ifelse(data$Tsunami == "Strong", 1, 0))
data$Beach <- as.factor(ifelse(data$Beach == "Yes", 1, 0))

# Function to perform 2-fold cross-validation for NB
cross_val_NB <- function(data, m, p) {
  n <- nrow(data)
  folds <- 2
  accuracy <- numeric(folds)
  
  for (i in 1:folds) {
    # Create training and testing sets
    test_indices <- (1:n) %% folds == (i - 1)
    train_data <- data[!test_indices, ]
    test_data <- data[test_indices, ]
    
    # Train Naive Bayes model
    nb_model <- naiveBayes(Beach ~ ., data = train_data)
    
    # Make predictions on the testing set
    nb_pred_probs <- predict(nb_model, test_data, type = "raw", class = 1)
    
    # Extract class probabilities for "Yes" class
    nb_class_probs <- nb_pred_probs$posterior[, "Yes"]
    
    # Convert probabilities to class predictions
    nb_class_pred <- ifelse(nb_class_probs > 0.5, "Yes", "No")
    
    # Calculate accuracy
    accuracy[i] <- sum(nb_class_pred == test_data$Beach) / nrow(test_data)
    
    # Display NB model for the current fold
    cat("Naive Bayes Model (Fold", i, "):\n")
    print(nb_model)
    cat("\n")
  }
  
  # Return average accuracy across folds
  return(mean(accuracy))
}

# Function to perform 2-fold cross-validation for DT
cross_val_DT <- function(data) {
  n <- nrow(data)
  folds <- 2
  accuracy <- numeric(folds)
  
  for (i in 1:folds) {
    # Create training and testing sets
    test_indices <- (1:n) %% folds == (i - 1)
    train_data <- data[!test_indices, ]
    test_data <- data[test_indices, ]
    
    # Train Decision Tree model
    dt_model <- rpart(Beach ~ ., data = train_data, method = "class")
    
    # Make predictions on the testing set
    dt_pred <- predict(dt_model, newdata = test_data, type = "class")
    
    # Calculate accuracy
    accuracy[i] <- sum(dt_pred == test_data$Beach) / nrow(test_data)
    
    # Display DT model for the current fold
    cat("Decision Tree Model (Fold", i, "):\n")
    print(dt_model)
    cat("\n")
  }
  
  # Return average accuracy across folds
  return(mean(accuracy))
}

# Set m and p values for NB
m_value <- 0.001
p_value <- 0.5

# Perform 2-fold cross-validation for NB
nb_accuracy <- cross_val_NB(data, m_value, p_value)
cat("Average 2-fold Cross-Validation Accuracy for Naive Bayes:", nb_accuracy, "\n\n")

# Perform 2-fold cross-validation for DT
dt_accuracy <- cross_val_DT(data)
cat("Average 2-fold Cross-Validation Accuracy for Decision Tree:", dt_accuracy, "\n\n")




###   TRYING NB A DIFFERENT WAY
m_value <- 0.001
p_value <- 0.5

# Train Naive Bayes model with custom parameters
nb_model <- naive_bayes(Beach ~ ., data = data, laplace = m_value, usekernel = FALSE, adjust = p_value)

# Make predictions
nb_pred <- predict(nb_model, newdata = data)

nb_class_pred <- attr(nb_pred, "class")

# Calculate accuracy
accuracy <- sum(nb_class_pred == data$Beach) / nrow(data)

# Display the Naive Bayes model
print(nb_model)

# Display accuracy
cat("Accuracy for Naive Bayes:", accuracy, "\n")



