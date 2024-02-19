# prep space and load data
library(ISLR)
library(splines)

data(Wage)
dim(Wage)
names(Wage)
attach(Wage)
W1=summary(Wage)



# wage vs age
agelims=range(age)
age.grid=seq(from=agelims[1], to=agelims[2])

fit2 = lm(wage~ns(age, df=4), data=Wage)
pred=predict(fit2, newdata=list(age=age.grid), se=T)

plot(x=age,y=wage,col="gray")
title("Natural Cubic Splines", outer=T)
lines(age.grid,pred$fit,lwd=2, col="red")
lines(age.grid,pred$fit+2*pred$se, lty="dashed")
lines(age.grid,pred$fit-2*pred$se, lty="dashed")



# wage vs year
yearlims=range(year)
year.grid=seq(from=yearlims[1], to=yearlims[2])

fit2 = lm(wage~ns(year, df=4), data=Wage)
pred=predict(fit2, newdata=list(year=year.grid), se=T)

plot(x=year,y=wage,col="gray")
title("Natural Cubic Splines", outer=T)
lines(year.grid,pred$fit,lwd=2, col="red")
lines(year.grid,pred$fit+2*pred$se, lty="dashed")
lines(year.grid,pred$fit-2*pred$se, lty="dashed")



# wage vs education
plot(x=education,y=wage,col="gray")
title("Natural Cubic Splines", outer=T)
lines(age.grid,pred$fit,lwd=2, col="red")
lines(age.grid,pred$fit+2*pred$se, lty="dashed")
lines(age.grid,pred$fit-2*pred$se, lty="dashed")









