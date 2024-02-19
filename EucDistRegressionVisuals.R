#Q3
train <- data.frame(observation=c(1,2,3,4,5,6), X1 = c(0,2,0,0,1,1),X2 = c(3,0,1,1,0,1),
                          X3 = c(0,0,3,2,1,1), Y = c("Red", "Red", "Red", "Green", "Green", "Red"),
                          Euclidean_distance = c(sqrt(3*3), sqrt(2*2),sqrt(1*1+3*3), sqrt(1*1+2*2),sqrt(1*1+1*1),sqrt(1*1+1*1+1*1)))
train

library(ggplot2)
ggplot(train, aes(x = Euclidean_distance, y = 0))+
  geom_point(aes(colour = Y))+
  scale_color_manual(values = c( "green", "red"))+
  xlim(0,4)

#Q6
library(ISLR)

lm.fit <- lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)

predict(lm.fit, data.frame(horsepower = c(85)), interval ="confidence")

predict(lm.fit, data.frame(horsepower = c(85)), interval ="prediction")

attach(Auto)
plot(mpg~horsepower, main =" MPG vs Horsepower", xlab = " Horsepower", ylab ="MPG")
abline(coef = coef(lm.fit), col ="red")
detach(Auto)

par(mfrow=c(2,2))
plot(lm.fit)

#Q7
library(tinytex)
library(MASS)

set.seed(1)
x = rnorm(100,0,1)
x

eps = rnorm(100,0,sqrt(0.25))
eps

y <- -1 + 0.5*x + eps
length(y)

plot(x,y)

fitteh = lm(y~x)
summary(fitteh)

par(mfrow=c(1,1))

plot(x,y)
abline(coef = c(-1,0.5),col = "blue")
abline(fit,col="red")
legend("topleft",c("ls","regression"),col=c("red","blue"),lty = c(1,2))

lm.fit2 <- lm(y ~ x + I(x^2))
summary(lm.fit2)

set.seed(1)
eps <- rnorm(100, sd = 0.125)
x <- rnorm(100)
y <- -1 + 0.5 * x + eps
plot(x, y)
lm.fit3 <- lm(y ~ x)
summary(lm.fit3)

abline(lm.fit3, col = "red")
abline(-1, 0.5, col = "blue")
legend("topleft", c("lm.fit3 Least square", "Regression"), col = c("red", "blue"), lty = c(1, 1))

set.seed(1)
eps <- rnorm(100, sd = 0.5)
x <- rnorm(100)
y <- -1 + 0.5 * x + eps
plot(x, y)
lm.fit4 <- lm(y ~ x)
summary(lm.fit4)

abline(lm.fit4, col = "red")
abline(-1, 0.5, col = "blue")
legend("topleft", c("lm.fit4 Least square", "Regression"), col = c("red", "blue"), lty = c(1, 1))

confint(lm.fit)

confint(lm.fit3)

confint(lm.fit4)







