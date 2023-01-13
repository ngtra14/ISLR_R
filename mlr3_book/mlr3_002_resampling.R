
# mlr3_001_intro.R --------------------------------------------------------

library(mlbench)

data("PimaIndiansDiabetes", package = "mlbench")


library(mlr3)
library(mlr3verse)
library(mlr3viz)
task_diabetes <- as_task_classif(PimaIndiansDiabetes, 
                                 target = "diabetes",
                                 positive = "pos")

# autoplot(task_diabetes, type = "duo")
mlr_learners$get("classif.log_reg")
learner = lrn("classif.log_reg", predict_type = "prob")

# resampling
resampling = rsmp("cv", folds = 3)

# split train and test data
set.seed(2022)
train_idx = sample(task_diabetes$row_ids, 0.67 * task_diabetes$nrow)
test_idx = setdiff(task_diabetes$row_ids, train_idx)

# train the data
learner$train(task_diabetes, row_ids = train_idx)

# predict on the test data
preds = learner$predict(task_diabetes, row_ids = test_idx)
preds

# evaluate the model
measure = msr("classif.acc")
preds$score(measure)

# confusion matrix
preds$confusion

# roc curve
autoplot(preds, type = "roc")

# performance test by resampling
rr = resample(task_diabetes, learner, resampling, store_models = TRUE)
print(rr)

# check the average performance
rr$aggregate((msr("classif.acc")))

# check the roc curve over 3 folds
autoplot(rr, type = "roc")

# find the auc values for three folds
autoplot(rr, measure = msr("classif.auc"))

# select two most important features
library(randomForest)
rf_diabetes <- randomForest(diabetes ~., data = PimaIndiansDiabetes[train_idx,])
pred_rf <- predict(rf_diabetes, PimaIndiansDiabetes[test_idx, ])
table(pred_rf, PimaIndiansDiabetes[test_idx, "diabetes"])
varImpPlot(rf_diabetes, sort = TRUE)
