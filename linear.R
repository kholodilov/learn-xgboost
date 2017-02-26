library(MASS)
library(ISLR)
library(car)

data(Boston)
names(Boston)

lm.fit = lm(medv~lstat, data=Boston)
lm.fit
summary(lm.fit)
coef(lm.fit)
confint(lm.fit)

predict(lm.fit, data.frame(lstat=c(5,10,5)),interval="confidence")
predict(lm.fit, data.frame(lstat=c(5,10,5)),interval="prediction")

par(mfrow=c(1,1))
plot(Boston$lstat, Boston$medv, col="red", pch="+")
abline(lm.fit, lwd=2)

par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


lm.fit = lm(medv~., data=Boston)
summary(lm.fit)
vif(lm.fit)

lm.fit = lm(medv~.-age, data=Boston)
summary(lm.fit)

lm.fit = lm(medv~lstat*age, data=Boston)
summary(lm.fit)

lm.fit = lm(medv~lstat+I(lstat^2), data=Boston)
summary(lm.fit)

anova(lm(medv~lstat, data=Boston), lm(medv~lstat+I(lstat^2), data=Boston))

lm.fit = lm(medv~poly(lstat, 5), data=Boston)
summary(lm.fit)

lm.fit = lm(medv~log(lstat)+poly(lstat, 5), data=Boston)
summary(lm.fit)

anova(lm(medv~poly(lstat, 5), data=Boston), lm(medv~log(lstat)+poly(lstat, 5), data=Boston))



data(Carseats)
names(Carseats)

lm.fit=lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc)
