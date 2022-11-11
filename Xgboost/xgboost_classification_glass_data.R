
# glass data classificaiton - xgboost -------------------------------------

library(xgboost)
library(mlbench)
data("Glass")
head(Glass)
dat <- Glass


# Redefine the labels -----------------------------------------------------

levels(dat$Type)
types <- dat$Type
label <- as.integer(types) - 1
plot(label)

dat$Type <- NULL
head(dat)


# Training and testing data set -------------------------------------------

n = nrow(dat)
train_index <- sample(n, floor(0.7 * n))
train_data <- as.matrix(dat[train_index,])
train_label <- label[train_index]

test_data <- as.matrix(dat[-train_index,])
test_label <- label[-train_index]


# Transform train and test data into xgb.DMatrix obj ----------------------

xgb_train <- xgb.DMatrix(data = train_data, label = train_label)
xgb_test <- xgb.DMatrix(data = test_data, label = test_label)


# Set up the xgboost model parameters -------------------------------------

num_class = length(levels(types))
params <- list(
  booster = "gbtree",
  eta = 0.001,
  max_depth = 5,
  gamma = 3,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = num_class
)


# Train the model ---------------------------------------------------------

xgb_fit=xgb.train(
  params=params,
  data=xgb_train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb_train,val2=xgb_test),
  verbose=0
)

xgb_fit


# Predictions on new data -------------------------------------------------

xgb_pred <- predict(xgb_fit, test_data, reshape = T)
xgb_pred <- as.data.frame(xgb_pred)
colnames(xgb_pred) <- levels(types)

# find the max prob for each three probabilities
xgb_pred$prediction <- apply(xgb_pred, 1, function(x) colnames(xgb_pred)[which.max(x)])
xgb_pred$label = levels(types)[test_label + 1]


# confusion matrix - results ----------------------------------------------

library(caret)
data(iris)
confusionMatrix(iris[-train_index, "Species"], as.factor(xgb_pred$label))

# simple version for observation
table(Glass[-train_index, "Type"], as.factor(xgb_pred$label))
