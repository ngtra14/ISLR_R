
# Load the package - xgboost ----------------------------------------------

require("xgboost")

# Load dataset ------------------------------------------------------------

data(agaricus.train, package = "xgboost")
data("agaricus.test", package = "xgboost")
train <- agaricus.train
test <- agaricus.test

# EDA ---------------------------------------------------------------------
str(train)
dim(train$data)
dim(test$data)
class(train$data)[1]
class(train$label)


# Basic training step -----------------------------------------------------
# sparse matrix as input data
bstSparse <- xgboost(data = train$data, 
                     label = train$label, 
                     max_depth = 2,
                     eta = 1, 
                     nthread = 2, 
                     nrounds = 2,
                     objective = "binary:logistic")

# dense matrix as in R environment
bstDense <- xgboost(data = as.matrix(train$data),
                    label = train$label,
                    max_depth = 2, 
                    eta = 1,
                    nthread = 2,
                    nrounds = 2,
                    objective = "binary:logistic")

# xgb.DMatrix format input/output
dtrain <- xgb.DMatrix(data = train$data, label = train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)
bstDMatrix <- xgboost(data = dtrain, 
                      max_depth = 2, 
                      eta = 1, 
                      nthread = 2, 
                      nrounds = 2, 
                      objective = "binary:logistic")

# verbose option
# verbose = 1, print evaluation metric
bst <- xgboost(data = dtrain, 
               max_depth = 2, 
               eta = 1, 
               nthread = 2, 
               nrounds = 2, 
               objective = "binary:logistic", 
               verbose = 1)


# Predictions -------------------------------------------------------------

pred <- predict(bst, test$data)
print(length(pred))
print(head(pred))


# Get binary classification results ---------------------------------------

prediction <- as.numeric(pred > 0.5)
print(head(prediction))


# Model performance -------------------------------------------------------

err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error: ", err))


# Linear boost model ------------------------------------------------------
watchlist <- list(train=dtrain, test=dtest)
bst <- xgb.train(data=dtrain, 
                 booster = "gblinear", 
                 max_depth=2, 
                 nthread = 2, 
                 nrounds=2, 
                 watchlist=watchlist, 
                 eval_metric = "error", 
                 eval_metric = "logloss", 
                 objective = "binary:logistic")


# View feature importance -------------------------------------------------

importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


# View the trees from model -----------------------------------------------

xgb.dump(bst, with_stats = TRUE)
# xgb.plot.tree(model=bst)
