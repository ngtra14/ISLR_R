# 16 Miscellaneous Model Functions ----------------------------------------
# 16.1 Yet Another k-Nearest Neighbor Function
library(caret)
library(mlbench)
data(Sonar)
set.seed(808)
inTrain <- createDataPartition(Sonar$Class, p = 2/3, list = FALSE)

## Save the predictors and class in different objects
sonarTrain <- Sonar[ inTrain, -ncol(Sonar)]
sonarTest  <- Sonar[-inTrain, -ncol(Sonar)]

trainClass <- Sonar[ inTrain, "Class"]
testClass  <- Sonar[-inTrain, "Class"]

centerScale <- preProcess(sonarTrain)
centerScale

training <- predict(centerScale, sonarTrain)
testing <- predict(centerScale, sonarTest)

knnFit <- knn3(training, trainClass, k = 11)
knnFit
predict(knnFit, head(testing), type = "prob")

# 16.4 Bagging
library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]
set.seed(825)
baggedCT <- bag(x = training[, names(training) != "Class"],
                y = training$Class,
                B = 50,
                bagControl = bagControl(fit = ctreeBag$fit,
                                        predict = ctreeBag$pred,
                                        aggregate = ctreeBag$aggregate))              
summary(baggedCT)
