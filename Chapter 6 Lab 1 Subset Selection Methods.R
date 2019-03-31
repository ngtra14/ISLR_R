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
plot(regfit.full,scale="Cp") # white blocks mean the predictors are 'out', black means 'in'
plot(regfit.full,scale="bic")
coef(regfit.full,6)
dev.off()
# Forward and Backward Stepwise Selection
regfit.fwd <- regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
plot(regfit.fwd, scale = "Cp")

regfit.bwd <- regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7) # not all coefs are same most often
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# Choosing Among Models

set.seed(1)
#train <- sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
train <- sample(seq(263), 180, replace = FALSE)
train

test <- (!train)
regfit.fwd <- regsubsets(Salary~.,data=Hitters[train,],nvmax=19, method = "forward")
x.test <- model.matrix(Salary~.,data=Hitters[-train,])
head(x.test) # looks like 'Salary' column is removed

val.errors <- rep(NA,19)
for(i in 1:19){
  coefi <- coef(regfit.fwd,id=i)
  pred <- x.test[,names(coefi)]%*%coefi
  val.errors[i] <- mean((Hitters$Salary[-train]-pred)^2)
}

plot(sqrt(val.errors), ylab = "Root MSE", ylim = c(300,400), pch=19, type = "b")
points(sqrt(regfit.fwd$rss[-1]/180), col="blue", pch=19, type='b')
legend("topright", legend=c("Training", "Validation"), col=c("blue", "black"), pch=19)

# find the 'best' model from the min errors
val.errors
which.min(val.errors)
dev.off()
plot(val.errors) # plot to review, more convenient
coef(regfit.fwd,10)

predict.regsubsets <- function(object,newdata,id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best <- regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

# 10 fold cross valication
k <- 10
set.seed(11)
folds <- sample(1:k,nrow(Hitters),replace=TRUE)
folds
table(folds)

cv.errors <- matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
cv.errors # take a look at the matrix built

for(j in 1:k){
  best.fit <- regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19, method="forward")
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
coef(reg.best,10)
