## PCA followd by Logistic regression
# https://stackoverflow.com/questions/10876040/principal-component-analysis-in-r
set.seed(1)
want <- sample(50, 40)
Iris <- iris[c(51:100, 101:150), ] ## only keep versicolor and virginica

## take our training and test sets
train <- droplevels(Iris[c((1:50)[want], (51:100)[want]), , drop = FALSE])
test <- droplevels(Iris[c((1:50)[-want], (51:100)[-want]), , drop = FALSE])

## fit the PCA
pc <- prcomp(train[, 1:4])

## create data frame for logistic regression
mydata <- data.frame(Species = train[, "Species"], pc$x)
head(mydata)

## fit the model
mod <- glm(Species ~ PC1, data = mydata, family = binomial)

# Predict the scores on PC1 for the test set data
head(test)
test.p <- predict(pc, newdata = test[, 1:4])
head(test.p)

# Now use that to predict the class
pred <- predict(mod, newdata = data.frame(test.p), type = "response")

# cross-classification table 
predSpecies <- factor(ifelse(pred >= 0.5, "virginica", "versicolor"))
table(test$Species, predSpecies)
