# Load required library
library(e1071)
library(rpart)

data <- read.csv("/Users/jacobwatson/Downloads/6115IP.csv")

data$Thunder <- as.numeric(data$Thunder == "Sunny")
data$Hailstorm <- as.numeric(data$Hailstorm == "Hot")
data$Homework <- as.numeric(data$Homework == "High")
data$Tsunami <- as.numeric(data$Tsunami == "Strong")
data$Beach <- as.numeric(data$Beach == "Yes")

# Function to calculate m-estimate probability for NB
m_estimate <- function(x, m, p) {
  return((sum(x) + m * p) / (length(x) + m))
}

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
    nb_pred <- predict(nb_model, newdata = test_data)
    
    # Calculate accuracy
    accuracy[i] <- sum(nb_pred$class == test_data$Beach) / nrow(test_data)
    
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

# Compare the accuracies and choose the better classifier
if (nb_accuracy > dt_accuracy) {
  cat("Naive Bayes is chosen as the better classifier.\n\n")
  # Train the final Naive Bayes model
  final_nb_model <- naiveBayes(Beach ~ ., data = data)
  cat("Final Naive Bayes Model:\n")
  print(final_nb_model)
} else {
  cat("Decision Tree is chosen as the better classifier.\n\n")
  # Train the final Decision Tree model
  final_dt_model <- rpart(Beach ~ ., data = data, method = "class")
  cat("Final Decision Tree Model:\n")
  print(final_dt_model)
}
