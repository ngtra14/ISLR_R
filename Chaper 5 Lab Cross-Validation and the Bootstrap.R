# Chaper 5 Lab: Cross-Validation and the Bootstrap

# The Validation Set Approach

library(ISLR)
require(boot)

# plot the mpg ` horsepower first
plot(mpg ~ horsepower, data=Auto)

set.seed(1)
train <- sample(392,196)
lm.fit <- lm(mpg~horsepower,data=Auto,subset=train)

attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2)
train <- sample(392,196)
lm.fit <- lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# Leave-One-Out Cross-Validation

glm.fit <- glm(mpg~horsepower,data=Auto)
coef(glm.fit)

lm.fit <- lm(mpg~horsepower,data=Auto)
coef(lm.fit)

library(boot)
glm.fit <- glm(mpg~horsepower,data=Auto)
cv.err <- cv.glm(Auto,glm.fit)
cv.err$delta

cv.error <- rep(0,5)
degree <- 1:5
for (i in degree){
  glm.fit <- glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

# k-Fold Cross-Validation

set.seed(17)
# --- original codes may not produce what's in the video lecture
# cv.error.10 <- rep(0,10)
# for (i in 1:10){
#   glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
#   cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
# }
# cv.error.10

# --- modifed as follows ---
cv.error.10 <- rep(0,5)
degree <- 1:5
for (i in degree){
  glm.fit <- glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i] <- cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

# plot the error trend
plot(degree, cv.error, type = "b")
lines(degree, cv.error.10, type = "b", col="red")
cv.error.10

# lets write a simple function to use formula
loocv <- function(fit){
  h <- lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

# run on the fitted model
loocv(glm.fit)

# The Bootstrap
# check the data set first
head(Portfolio) # only has X, Y 
str(Portfolio)

alpha.fn <- function(data,index){
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)

# another way is to build 2 functions first to get alpha, second to get alpha for all the data

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
# length(unique(sample(100, 100, replace = T))) # check the samples with replacement
boot.out <- boot(Portfolio,alpha.fn,R=1000)
plot(boot.out)

# Estimating the Accuracy of a Linear Regression Model
boot.fn <- function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))

boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

# to run 1000 times 
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef

boot.fn <- function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))

set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
