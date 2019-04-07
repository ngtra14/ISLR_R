# Chapter 6 Lab 1: Subset Selection Methods
# http://www-bcf.usc.edu/~gareth/ISL/code.html
# comments and syntax and extra illustrations are added by "PyRPy"
# Best Subset Selection

library(ISLR)
# fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
head(Hitters)

library(leaps)
# The goal is to predict salaries for each player
regfit.full <- regsubsets(Salary~.,Hitters)
summary(regfit.full)

regfit.full <- regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary <- summary(regfit.full)
names(reg.summary)

reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20) # put a red point on the plot

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)

which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp") # white blocks mean the predictors are 'in', black means 'out'
plot(regfit.full,scale="bic")
coef(regfit.full,6)

# Forward and Backward Stepwise Selection

regfit.fwd <- regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7) # not all coefs are same most often
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# Choosing Among Models

set.seed(1)
train <- sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test <- (!train)
regfit.best <- regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat <- model.matrix(Salary~.,data=Hitters[test,])
head(test.mat) # looks like 'Salary' column is removed

val.errors <- rep(NA,19)
for(i in 1:19){
  coefi <- coef(regfit.best,id=i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((Hitters$Salary[test]-pred)^2)
}

# find the 'best' model from the min errors
val.errors
which.min(val.errors)
dev.off()
plot(val.errors) # plot to review, more convenient
coef(regfit.best,10)

predict.regsubsets <- function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best <- regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

# 10 fold cross valication
k <- 10
set.seed(1)
folds <- sample(1:k,nrow(Hitters),replace=TRUE)
folds

cv.errors <- matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
cv.errors # take a look at the matrix built

for(j in 1:k){
  best.fit <- regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred <- predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i] <- mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}
cv.errors # take a look at errors for each model after running 19 x 10 models

mean.cv.errors <- apply(cv.errors,2,mean) # find average error for model with same number of variables
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best <- regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)


# Chapter 6 Lab 2: Ridge Regression and the Lasso

x <- model.matrix(Salary~.,Hitters)[,-1]
y <- Hitters$Salary

# Ridge Regression

library(glmnet)
grid <- 10^seq(10,-2,length=100) # to produce a squence of 100 numbers between 10 and -2 
ridge.mod <- glmnet(x,y,alpha=0,lambda=grid) # least square 

dim(coef(ridge.mod))
ridge.mod$lambda[50]

coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50,type="coefficients")[1:20,]

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

ridge.mod <- glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred <- predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

ridge.pred <- predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred <- predict(ridge.mod,s=0,newx=x[test,],exact=T) # this is not working
# Error: used coef.glmnet() or predict.glmnet() with `exact=TRUE` so must in addition supply 
# original argument(s)  x and y  in order to safely rerun glmnet
mean((ridge.pred-y.test)^2)

lm(y~x, subset=train)
predict(ridge.mod,s=0, type="coefficients")[1:20,] # removed exact=T

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

lasso.mod <- glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out <- glmnet(x,y,alpha=1,lambda=grid)
lasso.coef <- predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]


# Chapter 6 Lab 3: PCR and PLS Regression

# Principal Components Regression

library(pls)
set.seed(2)
pcr.fit <- pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)

validationplot(pcr.fit,val.type="MSEP")

set.seed(1)
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
pls.pred <- predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)

pls.fit <- plsr(Salary~., data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)
