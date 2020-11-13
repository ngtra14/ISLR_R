# 15 Variable Importance --------------------------------------------------
# 15.1 Model Specific Metrics
# 15.2 Model Independent Metrics

# not working yet with Fit3
gbmImp <- varImp(gbmFit3, scale = FALSE)
gbmImp

library(mlbench)
data(Sonar)
library(gbm)
library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

# with random search
svmControl <- trainControl(method = "repeatedcv",
                           number = 10, repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "random")
set.seed(825)
svmFit <- train(Class ~ ., data = training,
                method = "svmRadial", 
                trControl = svmControl, 
                preProc = c("center", "scale"),
                metric = "ROC",
                tuneLength = 15)

roc_imp <- filterVarImp(x = training[, -ncol(training)], y = training$Class)
head(roc_imp)

roc_imp2 <- varImp(svmFit, scale = FALSE)
roc_imp2

# example for imp from Chapter 5
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

set.seed(825)
gbmFit3 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = "ROC")
gbmFit3
gbmImp <- varImp(gbmFit3, scale = FALSE) # include 'gbm' library to fix it !
# https://stackoverflow.com/questions/50111055/gradient-boosting-variable-importance
plot(gbmImp, top = 20)
