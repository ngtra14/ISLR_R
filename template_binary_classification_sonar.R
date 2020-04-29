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
pred <- ifelse(prob > 0.5, "R", "M")
table(testing$Class, pred)
confusionMatrix(testing$Class, as.factor(pred))

