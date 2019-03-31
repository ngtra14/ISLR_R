# Chapter 6 Lab 3: PCR and PLS Regression

library(ISLR)
library(leaps)
Hitters <- na.omit(Hitters) # fix problems with unequal dim of x and y
x <- model.matrix(Salary~.-1, data=Hitters)
y <- Hitters$Salary

# Principal Components Regression

library(pls)
set.seed(2)
pcr.fit <- pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)

validationplot(pcr.fit,val.type="MSEP")

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

pcr.fit <- pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")

pcr.pred <- predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)

pcr.fit <- pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

# Partial Least Squares

set.seed(1)
pls.fit <- plsr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)

validationplot(pls.fit,val.type="MSEP")

pls.pred <- predict(pls.fit,x[test,],ncomp=2) # something not correct in dimensions
mean((pls.pred-y.test)^2)

pls.fit <- plsr(Salary~., data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)
