# Load required library
library(e1071)

data <- read.csv("/Users/jacobwatson/Downloads/6115IP.csv")

# Convert categorical variables to factors
data$Thunder <- as.factor(data$Thunder)
data$Hailstorm <- as.factor(data$Hailstorm)
data$Homework <- as.factor(data$Homework)
data$Tsunami <- as.factor(data$Tsunami)
data$Beach <- as.factor(data$Beach)

# Function to perform cross-validation for NB
cross_val_NB <- function(data, folds = 2) {
  n <- nrow(data)
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

# Set the number of folds
num_folds <- 2

# Perform cross-validation for NB using a loop
nb_accuracies <- numeric(num_folds)
for (j in 1:num_folds) {
  start_idx <- ((j - 1) * nrow(data) / num_folds) + 1
  end_idx <- j * (nrow(data) / num_folds)
  
  subset_data <- data[start_idx:end_idx, ]
  nb_accuracies[j] <- cross_val_NB(subset_data)
}

# Average accuracy across different segments
avg_nb_accuracy <- mean(nb_accuracies)
cat("Average Cross-Validation Accuracy for Naive Bayes:", avg_nb_accuracy, "\n")

