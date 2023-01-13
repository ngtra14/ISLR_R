
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
