# 14 Adaptive Resampling --------------------------------------------------

library(mlbench)
data(Sonar)
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

# Adaptive Resampling
adaptControl <- trainControl(method = "adaptive_cv",
                             number = 10, repeats = 10,
                             adaptive = list(min = 5, alpha = 0.05, 
                                             method = "gls", complete = TRUE),
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary,
                             search = "random")

set.seed(825)
svmAdapt <- train(Class ~ ., data = training,
                  method = "svmRadial", 
                  trControl = adaptControl, 
                  preProc = c("center", "scale"),
                  metric = "ROC",
                  tuneLength = 15)
svmAdapt
