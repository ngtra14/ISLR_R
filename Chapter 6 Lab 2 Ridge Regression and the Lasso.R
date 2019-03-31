# Chapter 6 Lab 2: Ridge Regression and the Lasso
# http://www-bcf.usc.edu/~gareth/ISL/code.html
# comments and syntax and extra illustrations are added by "PyRPy"

library(ISLR)
library(leaps)
Hitters <- na.omit(Hitters) # fix problems with unequal dim of x and y
x <- model.matrix(Salary~.-1, data=Hitters)
y <- Hitters$Salary

set.seed(1)
#train <- sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
train <- sample(seq(263), 180, replace = FALSE)
# Ridge Regression

library(glmnet)
# grid <- 10^seq(10,-2,length=100) # to produce a squence of 100 numbers between 10 and -2 
# ridge.mod <- glmnet(x,y,alpha=0,lambda=grid) # least square 

fit.ridge <- glmnet(x, y, alpha = 0)
plot(fit.ridge, xvar='lambda', label=TRUE)
cv.ridge <- cv.glmnet(x,y, alpha=0)
plot(cv.ridge)

# dim(coef(ridge.mod))
# ridge.mod$lambda[50]
# 
# coef(ridge.mod)[,50]
# sqrt(sum(coef(ridge.mod)[-1,50]^2))
# 
# ridge.mod$lambda[60]
# coef(ridge.mod)[,60]
# sqrt(sum(coef(ridge.mod)[-1,60]^2))
# 
# predict(ridge.mod,s=50,type="coefficients")[1:20,]
# 
# set.seed(1)
# train <- sample(1:nrow(x), nrow(x)/2)
# test <- (-train)
# y.test <- y[test]

# ridge.mod <- glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
# ridge.pred <- predict(ridge.mod,s=4,newx=x[test,])
# mean((ridge.pred-y.test)^2)
# mean((mean(y[train])-y.test)^2)
# 
# ridge.pred <- predict(ridge.mod,s=1e10,newx=x[test,])
# mean((ridge.pred-y.test)^2)
# 
# ridge.pred <- predict(ridge.mod,s=0,newx=x[test,],exact=T) # this is not working
# # Error: used coef.glmnet() or predict.glmnet() with `exact=TRUE` so must in addition supply 
# # original argument(s)  x and y  in order to safely rerun glmnet
# mean((ridge.pred-y.test)^2)
# 
# lm(y~x, subset=train)
# predict(ridge.mod,s=0, type="coefficients")[1:20,] # removed exact=T

set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out <- glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# The Lasso

fit.lasso <- glmnet(x,y)
plot(fit.lasso, xvar = "lambda", label = TRUE)
cv.lasso <- cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)

# use train and validation data sets to select lambda
lasso.tr <- glmnet(x[train,], y[train])
lasso.tr
pred <- predict(lasso.tr, x[-train,])
dim(pred)
rmse <- sqrt(apply((y[-train]-pred)^2, 2, mean))
plot(log(lasso.tr$lambda), rmse, type = 'b', xlab = "Log(lambda)")
lam.best <- lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr, s=lam.best)

# lasso.mod <- glmnet(x[train,],y[train],alpha=1,lambda=grid)
# plot(lasso.mod)
# 
# set.seed(1)
# cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
# plot(cv.out)
# bestlam <- cv.out$lambda.min
# lasso.pred <- predict(lasso.mod,s=bestlam,newx=x[test,])
# mean((lasso.pred-y.test)^2)
# out <- glmnet(x,y,alpha=1,lambda=grid)
# lasso.coef <- predict(out,type="coefficients",s=bestlam)[1:20,]
# lasso.coef
# lasso.coef[lasso.coef!=0]