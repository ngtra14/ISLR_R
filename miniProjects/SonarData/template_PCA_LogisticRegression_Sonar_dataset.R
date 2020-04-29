# Sonar dataset - binary classification -----------------------------------
library(caret)
library(mlbench)
data(Sonar)

head(Sonar)
table(Sonar$Class)
Sonar$Class <- as.factor(Sonar$Class)


# Split dataset -----------------------------------------------------------

set.seed(107)

inTrain <- createDataPartition(y=Sonar$Class,
                               p = 0.75,
                               list = FALSE)

training <- Sonar[inTrain, ]
testing <- Sonar[-inTrain, ]
nrow(training)
nrow(testing)


# Logistic regression -----------------------------------------------------

logReg <- glm(Class ~. , data = training, family = "binomial")


# Prediction and performance ----------------------------------------------

prob <- predict(logReg, testing, type = "response")
pred <- ifelse(prob > 0.5, "M", "R")
table(testing$Class, pred)
confusionMatrix(testing$Class, as.factor(pred))


# Using PCA preprocessing data --------------------------------------------

pca.out <- prcomp(training[, c(-61)],
                  center = TRUE,
                  scale. = TRUE)

biplot(pca.out, scale = 0)
pca.var <- pca.out$sdev^2

pve <- pca.var/sum(pca.var)
plot(pve, xlab = "Principal component", 
     ylab = "Proportion of variation explained",
     ylim = c(0, 1), 
     type = 'b')

plot(cumsum(pve), xlab = "Principal component", 
     ylab = "Accumulative Prop. of variation explained",
     ylim = c(0, 1), 
     type = 'b')


# Construct new data frame with PCA selected compoents 1:10 ---------------

mydata <- data.frame(Class = training[, "Class"], pca.out$x[, 1:20])
head(mydata)


# Fit the logistic regressin model again ----------------------------------

logRegPCA <- glm(Class ~ .,  data = mydata, family = binomial)

# Predict the scores on PC1 for the test set data
head(testing)
test.p <- predict(pca.out, newdata = testing[, 1:60])
head(test.p)

# Now use that to predict the class
pred <- predict(logRegPCA, newdata = data.frame(test.p[, 1:20]), type = "response")

# cross-classification table 
predClass <- factor(ifelse(pred >= 0.5, "M", "R"))
table(testing$Class, predClass)
confusionMatrix(testing$Class, as.factor(predClass))
