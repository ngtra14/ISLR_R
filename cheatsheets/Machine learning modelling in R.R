# Machine learning modelling in R - cheetsheet summary
# ref thertrader.com
data(iris)
head(iris)

# NaiveBayes classifer ----------------------------------------------------

library(e1071)
m <- naiveBayes(Species ~., data = iris)
table(predict(m, iris), iris[, 5])


# knn classifer -----------------------------------------------------------
table(iris$Species)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
train <- iris[ind == 1, ]
test <- iris[ind == 2, ]
train.X <- train[, -5]
test.X <- test[, -5]

train.Label <- train$Species
test.Label <- test$Species
library(class)
test_pred1 <- knn(train = train.X, test = test.X, 
                  cl = train.Label, k = 3, prob = TRUE)
table(test_pred1, test.Label)
dim(test)
# ref https://rpubs.com/Drmadhu/IRISclassification
