# K-Nearest Neighbors

library(class)
library(MASS)
library(tidyverse)

train <- (Smarket$Year<2005)
Smarket.2005 <- Smarket[!train,]
Direction.2005 <- Smarket$Direction[!train]

attach(Smarket)
train.X <- cbind(Lag1,Lag2)[train,]
test.X <- cbind(Lag1,Lag2)[!train,]
train.Direction <- Direction[train]

# select clashes with each other when loading MASS and dplyr at the same time !!!

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction,k=1)
table(knn.pred, Direction.2005) # 0.5
(83+43)/252

# increase k to 3 from 1
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