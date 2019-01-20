# Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN

# The Stock Market Data

library(ISLR)
library(tidyverse)
# EDA
glimpse(Smarket)
head(Smarket)

# names(Smarket)
# dim(Smarket)
# summary(Smarket)

pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

# Use ggplot and others
qplot(data=Smarket, y=Volume)

# Logistic Regression

glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

# Prediction
glm.probs <- predict(glm.fits,type="response")
glm.probs[1:10]
contrasts(Direction)

glm.pred <- rep("Down",1250)
glm.pred[glm.probs>.5] <- "Up"

table(glm.pred,Direction)
(507+145)/1250  # 0.5216, slightly better than random guess

# Accuracy
mean(glm.pred==Direction)

# Split into "train" and "test" data set
train <- (Year<2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)

Direction.2005 <- Direction[!train]
glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs <- predict(glm.fits,Smarket.2005,type="response")
glm.pred <- rep("Down",252)
glm.pred[glm.probs>.5] <- "Up"

table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

glm.fits <- glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs <- predict(glm.fits,Smarket.2005,type="response")
glm.pred <- rep("Down",252)
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005) #  0.5595238
106/(106+76)

predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

# Linear Discriminant Analysis

library(MASS)
lda.fit <- lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005) # 0.5595238
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)

# Quadratic Discriminant Analysis

qda.fit <- qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class <- predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005) # 0.5992063

# K-Nearest Neighbors

library(class)
train.X <- cbind(Lag1,Lag2)[train,]
test.X <- cbind(Lag1,Lag2)[!train,]

train.Direction <- Direction[train]
set.seed(1)
knn.pred <- knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005) # 0.5
(83+43)/252

knn.pred <- knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005) # 0.5357143

# Detach Smarket data set !!!
detach(Smarket)

# An Application to Caravan Insurance Data
head(Caravan)
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
glimpse(Caravan)

# Scale the data sets
standardized.X <- scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
9/(68+9)

knn.pred <- knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26

knn.pred <- knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15

glm.fits <- glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs <- predict(glm.fits,Caravan[test,],type="response")
glm.pred <- rep("No",1000)
glm.pred[glm.probs>.5] <- "Yes"
table(glm.pred,test.Y)
glm.pred<- rep("No",1000)
glm.pred[glm.probs>.25] <- "Yes"
table(glm.pred,test.Y)
11/(22+11)