# 9 Parallel Processing ---------------------------------------------------

library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

## All subsequent models are then run in parallel
# model <- train(y ~ ., data = training, method = "rf")

# 5.2 An Example
library(mlbench)
data(Sonar)
str(Sonar[, 1:10])

library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

# 5.3 Basic Parameter Tuning
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(825)
gbmFit1 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1
## When you are done:
stopCluster(cl)
