# LDA for multi-class classification on iris data -------------------------

# load the data set
data(iris)
head(iris)

# load the library MASS
library(MASS)

# Construct model formula
n = nrow(iris)
set.seed(2022)
train_index <- sample(n, floor(0.7 * n))
fit_lda <- lda(Species ~ Sepal.Length + Sepal.Width + 
                         Petal.Length + Petal.Width,
                         data = iris[train_index, ])
pred_lda <- predict(fit_lda, newdata = iris[-train_index, ])

pred_lda$class
table(iris[-train_index, ]$Species, pred_lda$class)

library(caret)
confusionMatrix(iris[-train_index, "Species"], as.factor(pred_lda$class))
