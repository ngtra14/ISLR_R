# Machine learning modelling in R - cheetsheet summary
# ref thertrader.com
data(iris)
head(iris)

# NaiveBayes classifer ----------------------------------------------------

library(e1071)
m <- naiveBayes(Species ~., data = iris)
table(predict(m, iris), iris[, 5])
