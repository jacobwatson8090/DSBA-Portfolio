library(ISLR)
install.packages("ISLR2")
library(ISLR2)

#### GENERATE VISUALIZATIONS

library(ggplot2)

df <- data.frame(x = seq(-4, 4, by = 2))
plot <- ggplot(df, aes(x = x)) +
geom_vline(xintercept = 0, linetype = "dashed") +
theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
xlim(-4, 4) +
geom_line(data = , color = "green") +
geom_line(data = , color = "purple")

print(plot)

plot <- ggplot(df, aes(x=x)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.25) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  xlim(-3, 4) +
  ylim(0, 5) 

print(plot)



### PRACTICE LAB 4.7
library(ISLR2)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[, -9])
attach(Smarket)
plot(Volume)

glm.fits <- glm(
  Direction ∼ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket family = binomial)

summary(glm.fits)

coef(glm.fits)

summary(glm.fits)$coef

summary(glm.fits)$coef[, 4]

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]

contrasts(Direction)

glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction)

mean(glm.pred == Direction)

train <- (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)

Direction.2005 <- Direction[!train]

glm.fits <- glm(
  Direction ∼ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket, family = binomial, subset = train
)

glm.probs <- predict(glm.fits, Smarket.2005,
                     type = "response")

glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"

table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

glm.fits <- glm(Direction ∼ Lag1 + Lag2, data = Smarket,
                family = binomial, subset = train)

glm.probs <- predict(glm.fits, Smarket.2005,
                     type = "response")

glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"

table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

predict(glm.fits,
        newdata =
          data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)),
        type = "response"
)

library(MASS)
lda.fit <- lda(Direction ∼ Lag1 + Lag2, data = Smarket,
                 subset = train)
lda.fit

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, Direction.2005)

mean(lda.class == Direction.2005)

sum(lda.pred$posterior[, 1] >= .5)

sum(lda.pred$posterior[, 1] < .5)

lda.pred$posterior[1:20, 1]
lda.class[1:20]

sum(lda.pred$posterior[, 1] > .9)

qda.fit <- qda(Direction ∼ Lag1 + Lag2, data = Smarket,
               subset = train)
qda.fit

qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)

mean(qda.class == Direction.2005)

library(e1071)
nb.fit <- naiveBayes(Direction ∼ Lag1 + Lag2, data = Smarket,
                       subset = train)
nb.fit

mean(Lag1[train][Direction[train] == "Down"])

sd(Lag1[train][Direction[train] == "Down"])

nb.class <- predict(nb.fit, Smarket.2005)
table(nb.class, Direction.2005)

mean(nb.class == Direction.2005)

nb.preds <- predict(nb.fit, Smarket.2005, type = "raw")
nb.preds[1:5, ]

library(class)
train.X <- cbind(Lag1, Lag2)[train, ]
test.X <- cbind(Lag1, Lag2)[!train, ]
train.Direction <- Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)

knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)

mean(knn.pred == Direction.2005)

dim(Caravan)

attach(Caravan)
summary(Purchase)

standardized.X <- scale(Caravan[, -86])
var(Caravan[, 1])

var(Caravan[, 2])

var(standardized.X[, 1])

var(standardized.X[, 2])

test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)

mean(test.Y != "No")

table(knn.pred, test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)

glm.fits <- glm(Purchase ∼ ., data = Caravan,
                family = binomial, subset = -test)

glm.probs <- predict(glm.fits, Caravan[test, ],
                     type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] <- "Yes"
table(glm.pred, test.Y)
test.Y
gpm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] <- "Yes"
table(glm.pred, test.Y)

attach(Bikeshare)
dim(Bikeshare)

names(Bikeshare)

mod.lm <- lm(
  bikers ∼ mnth + hr + workingday + temp + weathersit, data = Bikeshare
)
summary(mod.lm)

contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)
mod.lm2 <- lm(
  bikers ∼ mnth + hr + workingday + temp + weathersit,
  data = Bikeshare
)
summary(mod.lm2)

sum((predict(mod.lm) - predict(mod.lm2))^2)

all.equal(predict(mod.lm), predict(mod.lm2))

coef.months <- c(coef(mod.lm2)[2:12],
                 -sum(coef(mod.lm2)[2:12]))

plot(coef.months, xlab = "Month", ylab = "Coefficient",
     xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A",
                                       "M", "J", "J", "A", "S", "O", "N", "D"))

coef.hours <- c(coef(mod.lm2)[13:35],
                -sum(coef(mod.lm2)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient",
       col = "blue", pch = 19, type = "o")

mod.pois <- glm(
  bikers ~ mnth + hr + workingday + temp + weathersit,
  data = Bikeshare, family = poisson
)
summary(mod.pois)

coef.mnth <- c(coef(mod.pois)[2:12],
               -sum(coef(mod.pois)[2:12]))
plot(coef.mnth, xlab = "Month", ylab = "Coefficient",
       xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J"
                                       , "J", "A", "S", "O", "N", "D"))
coef.hours <- c(coef(mod.pois)[13:35],
                  -sum(coef(mod.pois)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient",
       col = "blue", pch = 19, type = "o")

plot(predict(mod.lm2), predict(mod.pois, type = "response"))
abline(0, 1, col = 2, lwd = 3)
































































