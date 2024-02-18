data <- read.csv("/Users/jacobwatson/Downloads/6115IP.csv")



set.seed(123)  
folds <- cut(seq(1, nrow(data)), breaks = 2, labels = FALSE)

#cv function
perform_cross_validation <- function(data, folds) {
  fold_results <- list()
  
  for (i in 1:max(folds)) {
    test_indices <- which(folds == i)
    train_indices <- setdiff(1:length(folds), test_indices)
    
    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]
    
    fold_results[[i]] <- list(train_data = train_data, test_data = test_data)
  }
  
  return(fold_results)
}

cross_validation_results <- perform_cross_validation(data, folds)

library(e1071)


train_and_evaluate_nb <- function(train_data, test_data) {
  model <- naiveBayes(Beach ~ Thunder + Hailstorm + Homework + Tsunami, data = train_data, laplace = 0.001)
  
  predictions <- predict(model, test_data)
  
  accuracy <- sum(predictions == test_data$Beach) / nrow(test_data)
  
  return(list(model = model, accuracy = accuracy))
}

nb_results <- lapply(cross_validation_results, function(fold) {
  train_and_evaluate_nb(fold$train_data, fold$test_data)
})

library(rpart)

train_and_evaluate_dt <- function(train_data, test_data) {
  model <- rpart(Beach ~ Thunder + Hailstorm + Homework + Tsunami, data = train_data, method = "class", parms = list(split = "information"))
  
  predictions <- predict(model, test_data, type = "class")
  
  accuracy <- sum(predictions == test_data$Beach) / nrow(test_data)
  
  return(list(model = model, accuracy = accuracy))
}

data$Thunder <- factor(data$Thunder, levels = c("Overcast", "Sunny"))

dt_results <- lapply(cross_validation_results, function(fold) {
  train_and_evaluate_dt(fold$train_data, fold$test_data)
})

nb_avg_accuracy <- mean(sapply(nb_results, function(result) result$accuracy))
dt_avg_accuracy <- mean(sapply(dt_results, function(result) result$accuracy))

if (nb_avg_accuracy > dt_avg_accuracy) {
  final_model <- nb_results[[1]]$model  
} else {
  final_model <- dt_results[[1]]$model  
}

print(final_model)


