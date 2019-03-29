# Chapter 3 Lab: Linear Regression

library(MASS)
library(ISLR)

# Simple Linear Regression

# fix(Boston)
str(Boston)
names(Boston)

# plot it first
plot(medv ~ lstat, Boston)

# attach(Boston)
# lm.fit <- lm(medv~lstat)
lm.fit <- lm(medv~lstat,data=Boston)
# attach(Boston)
# lm.fit <- lm(medv~lstat)
lm.fit
summary(lm.fit)

names(lm.fit)
coef(lm.fit)
confint(lm.fit)

# new predictions
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")

# plot the fitted line for linear model
library(ggplot2)

# one method
qplot(data=Boston, x=lstat,y=medv)+
  geom_smooth(method='lm', se=0)

# alternatively
qplot(data=Boston, x=lstat, y=medv)+
  geom_abline(slope = coef(lm.fit)[[2]], intercept = coef(lm.fit)[[1]], color='blue')

# abline(lm.fit),
# abline(lm.fit,lwd=3),
# abline(lm.fit,lwd=3,col="red")

# plot(lstat,medv,col="red")
# plot(lstat,medv,pch=20)
# plot(lstat,medv,pch="+")
# plot(1:20,1:20,pch=1:20)

# Multiple Linear Regression

lm.fit <- lm(medv~lstat+age,data=Boston)
summary(lm.fit)

lm.fit <- lm(medv~.,data=Boston)
summary(lm.fit)

# Leverge and outliers
# par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
dev.off()

# value inflation factor
library(car)
vif(lm.fit)

# steps to remove non-significant variables
lm.fit1 <- lm(medv~.-age,data=Boston)
summary(lm.fit1)
lm.fit1 <- update(lm.fit, ~.-age - indus)
summary(lm.fit1) # all coefs are significant

# Interaction Terms
summary(lm(medv~lstat*age,data=Boston))

# Non-linear Transformations of the Predictors
lm.fit2 <- lm(medv~lstat+I(lstat^2), data=Boston)
summary(lm.fit2)

# reduction fit evaluation
lm.fit <- lm(medv~lstat, data=Boston)
anova(lm.fit,lm.fit2)

# show all the metrics
par(mfrow=c(2,2))
plot(lm.fit2)

# plot the regression line 'quadratic' with easy R's own plot func
par(mfrow=c(1,1))
plot(medv ~ lstat, data = Boston)
points(Boston$lstat, fitted(lm.fit2), col='red', pch=20)

# lstat, power to five
lm.fit5 <- lm(medv~poly(lstat,5), data=Boston)
summary(lm.fit5)
points(Boston$lstat, fitted(lm.fit5), col='blue', pch=20)

# room transformed alone with log()
summary(lm(medv~log(rm),data=Boston))

# Qualitative Predictors

# fix(Carseats)
names(Carseats)
str(Carseats)
lm.fit <- lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

# attach(Carseats)
contrasts(Carseats$ShelveLoc)

# Writing Functions
regplot <- function(x,y){
  fit <- lm(y ~ x)
  plot(x, y)
  abline(fit, col='blue')
}

# plot Price ~ Sales from Carseats data set using regplot()
with(data=Carseats,
regplot(Sales, Price)
)

within(data=Carseats,
     regplot(Price, Sales)
)

# somethow it is not working...
regplot2 <- function(x,y, data4lm){
  fit <- lm(y ~ x, data=data4lm)
  plot(data4lm$x, data4lm$y)
  abline(fit, col='blue')
}

regplot2(Sales, Price, Carseats)

# LoadLibraries
LoadLibraries <- function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()
