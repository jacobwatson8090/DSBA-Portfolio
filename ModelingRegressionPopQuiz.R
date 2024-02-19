### CH 5 EXERCISE 6 - A ###
library(knitr)
library(ggthemes)
library(boot)
library(ISLR)
library(tidyverse)
library(broom)

set.seed(1)

log_1 <- glm(default ~ income + balance, data = Default, family = 'binomial')

tidy(log_1) %>%
  kable

### CH 5 EXERCISE 6 - B ###
boot.1 <- function(data = Default, index) {
  model1 <- glm(default ~ income + balance, data = data[index,], family = 'binomial')
  coef(model1)[2:3]
}

### CH 5 EXERCISE 6 - C ###
boot_vars <- boot(data = Default, boot.1, R = 200)

boot_vars %>%
  tidy %>%
  select(std.error) %>%
  kable










### CH 6 EXERCISE 9 - A ###
## Train/Test
library(caret)

data('College')
set.seed(1)

preTrain <- createDataPartition(College$Apps, p = 0.75, list = FALSE)

train <- College[preTrain,]
test <- College[-preTrain,]

obj <- preProcess(train, method = c('center', 'scale'))

training <- predict(obj, train)
testing <- predict(obj, test)

y_train <- training$Apps
y_test <- testing$Apps

encode <- dummyVars(Apps ~ ., data = training)
x_train <- predict(encode, training)
x_test <- predict(encode, testing)

### CH 6 EXERCISE 9 - B ###
lin1 <- lm(Apps ~ ., data = training)

pred <- predict(lin1, testing)

(lin1_stats <- postResample(pred, testing$Apps))

### CH 6 EXERCISE 9 - C ###
ridge <- train(x = x_train, y = y_train,
                   method = 'glmnet', 
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(alpha = 0,
                                          lambda = seq(0, 10e2, length.out = 20)))

(ridge_stats <- postResample(predict(ridge, x_test), y_test))

coef(ridge$finalModel, ridge$bestTune$lambda)

plot(ridge)

plot(varImp(ridge))

### CH 6 EXERCISE 9 - D ###
lasso <- train(x = x_train, y = y_train, 
                   method = 'glmnet',
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(alpha = 1,
                                          lambda = seq(0.0001, 1, length.out = 50)))

(lasso_stats <- postResample(predict(lasso, x_test), y_test))

coef(lasso$finalModel, lasso$bestTune$lambda)

plot(lasso)

plot(varImp(lasso))

### CH 6 EXERCISE 9 - E ###
pcr <- train(x = x_train, y = y_train,
                   method = 'pcr',
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(ncomp = 1:10))
(pcr_stats <- postResample(predict(pcr, x_test), y_test))

coef(pcr$finalModel)

plot(pcr)

plot(varImp(pcr))

### CH 6 EXERCISE 9 - F ###
pls <- train(x = x_train, y = y_train,
                   method = 'pls',
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(ncomp = 1:10))
(pls_stats <- postResample(predict(pls, x_test), y_test))

coef(pls$finalModel)

plot(pls)

### CH 6 EXERCISE 9 - G ###
as_data_frame(rbind(lin1_stats,
                    ridge_stats,
                    lasso_stats,
                    pcr_stats,
                    pls_stats)) %>%
  mutate(model = c('Linear', 'Ridge', 'Lasso', 'PCR', 'PLS')) %>%
  select(model, RMSE, Rsquared)

testing %>%
  summarize(sd = sd(Apps))












### CH 6 EXERCISE 11 - A ###
library(MASS)
library(leaps)
library(ggplot2)
library(glmnet)

## Train/Test
set.seed(1)
data('Boston')

preTrain <- createDataPartition(Boston$crim, p = 0.6, list = FALSE)

x_train <- Boston[preTrain, -1]
y_train <- Boston[preTrain, 1]
x_test <- Boston[-preTrain, -1]
y_test <- Boston[-preTrain, 1]

## Best Predictors
best_pred <- regsubsets(x = x_train, y = y_train, nvmax = 13)

bests <- summary(best_pred)

data_frame(MSE = bests$rss/nrow(x_train),
           Cp = bests$cp, 
           BIC = bests$bic,
           AdjustedR2 = bests$adjr2) %>%
  mutate(id = row_number()) %>%
  gather(Metric, value, -id) %>%
  ggplot(aes(id, value, col = Metric)) +
  geom_line() + geom_point() + ylab('') + 
  xlab('Number of Variables Used') + 
  facet_wrap(~ Metric, scales = 'free') +
  theme_tufte() + scale_x_continuous(breaks = 1:13)

scales = c('r2', 'adjr2', 'bic', 'Cp')
par(mfrow = c(2,2))

for (scale in scales) {
  plot(best_pred, scale = scale)
}

par(mfrow = c(1,1))

errors <- rep(NA,13)

test.mat <- model.matrix(crim ~ ., data = Boston[-preTrain,])

for (i in 1:13){
  coefs <- coef(best_pred, id=i)
  pred <- test.mat[,names(coefs)]%*%coefs
  errors[i] <- sqrt(mean((y_test - pred)^2))
}

data_frame(RMSE = errors) %>% 
  mutate(id = row_number()) %>% 
  ggplot(aes(id, RMSE)) +
  geom_line() + geom_point() + 
  xlab('Number of Variables Used') + 
  ggtitle('MSE on testing set') + 
  theme_tufte() + 
  scale_x_continuous(breaks = 1:13)

(subset_stats <- min(errors))

coef(best_pred, id = 1:5)

## Lasso Regression
lasso <- train(x = x_train, y = y_train,
                   method = 'glmnet',
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(alpha = 1,
                                          lambda = seq(0.001, 1, length.out = 100)),
                   preProcess = c('center', 'scale'))

(lasso_stats <- postResample(predict(lasso, x_test), y_test))

coef(lasso$finalModel, lasso$bestTune$lambda)

lasso$bestTune

plot(lasso)

plot(varImp(lasso))

## Ridge Regression
ridge <- train(x_train, y_train,
                   method = 'glmnet',
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(alpha = 0,
                                          lambda = seq(0, 1e2, length.out = 50)),
                   preProcess = c('center', 'scale'))
(ridge_stats <- postResample(predict(ridge, x_test), y_test))

coef(ridge$finalModel, ridge$bestTune$lambda)

ridge$bestTune

plot(ridge)

plot(varImp(ridge))

## Combined Lasso and Ridge with glmnet
glmnet <- train(x_train, y_train,
                    method = 'glmnet',
                    trControl = trainControl(method = 'cv', number = 10),
                    tuneGrid = expand.grid(alpha = seq(0, 1, length.out = 6),
                                           lambda = seq(0, 1e2, length.out = 20)),
                    preProcess = c('center', 'scale'))

(glmnet_stats <- postResample(predict(glmnet, x_test), y_test))

coef(object = glmnet$finalModel, s = glmnet$bestTune$lambda)

glmnet$bestTune

plot(glmnet)

plot(varImp(glmnet))

## Principal Components Regression
pcr <- train(x_train, y_train,
                 method = 'pcr',
                 trControl = trainControl(method = 'cv', number = 10),
                 tuneGrid = expand.grid(ncomp = 1:13), 
                 preProcess = c('center', 'scale'))

(pcr_stats <- postResample(predict(pcr, x_test), y_test))

plot(pcr)

plot(varImp(pcr))

## Partial Least Squares
pls <- train(x_train, y_train,
                 method = 'pls',
                 trControl = trainControl(method = 'cv', number = 10),
                 tuneGrid = expand.grid(ncomp = 1:13), 
                 preProcess = c('center', 'scale'))

(pls_stats <- postResample(predict(pls, x_test), y_test))

plot(varImp(pls))

### CH 6 EXERCISE 11 - B ###
rbind(c(subset_stats, NA, NA),
      lasso_stats, 
      ridge_stats,
      glmnet_stats,
      pcr_stats,
      pls_stats)






