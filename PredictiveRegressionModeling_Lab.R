## A
library(ISLR)
library(caret)
library(tidyverse)
install.packages("pls")
library(pls)
data('College')
set.seed(1)

Train1 <- createDataPartition(College$Apps, p = 0.75, list = FALSE)

train <- College[Train1,]
test <- College[-Train1,]

obj <- preProcess(train, method = c('center', 'scale'))

train <- predict(obj, train)
test <- predict(obj, test)

y_train <- train$Apps
y_test <- test$Apps

encode <- dummyVars(Apps ~ ., data = train)
x_train <- predict(encode, train)
x_test <- predict(encode, test)

## B
lin <- lm(Apps ~ ., data = train)

pred <- predict(lin, test)

(lin_stats <- postResample(pred, test$Apps))

## C
ridge <- train(x = x_train, y = y_train,
                   method = 'glmnet', 
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(alpha = 0,
                                          lambda = seq(0, 10e2, length.out = 20)))

(ridge_stats <- postResample(predict(ridge, x_test), y_test))

coef(ridge$finalModel, ridge$bestTune$lambda)

plot(ridge)

plot(varImp(ridge))

## D
lasso <- train(x = x_train, y = y_train, 
                   method = 'glmnet',
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(alpha = 1,
                                          lambda = seq(0.0001, 1, length.out = 50)))

(lasso_stats <- postResample(predict(lasso, x_test), y_test))

coef(lasso$finalModel, lasso$bestTune$lambda)

plot(lasso)

plot(varImp(lasso))

## E
pcr <- train(x = x_train, y = y_train,
                   method = 'pcr',
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(ncomp = 1:10))
(pcr_stats <- postResample(predict(pcr, x_test), y_test))

coef(pcr$finalModel)

plot(pcr)

plot(varImp(pcr))

## F
pls <- train(x = x_train, y = y_train,
                   method = 'pls',
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(ncomp = 1:10))
(pls_stats <- postResample(predict(pls, x_test), y_test))

coef(pls$finalModel)

plot(pls)

## G (Comparing)
as_data_frame(rbind(lin_stats,
                    ridge_stats,
                    lasso_stats,
                    pcr_stats,
                    pls_stats)) %>%
  mutate(model = c('Linear', 'Ridge', 'Lasso', 'PCR', 'PLS')) %>%
  select(model, RMSE, Rsquared)
