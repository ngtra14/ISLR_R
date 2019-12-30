# examples for using mlr3 -------------------------------------------------

library(mlr3)
task_iris <- TaskClassif$new(id = "iris", backend = iris, target = "Species")
task_iris

# load learner and set hyperparameter
learner <- lrn("classif.rpart", cp=0.01)

# train / test split
train_set <- sample(task_iris$nrow, 0.8*task_iris$nrow)
test_set <- setdiff(seq_len(task_iris$nrow), train_set)

# train the model
learner$train(task_iris, row_ids = train_set)

# predict data
prediction <- learner$predict(task_iris, row_ids = test_set)

# calculate performance
prediction$confusion

measure = msr("classif.acc")
prediction$score(measure)

# resample
# automatic resampling
resampling <- rsmp("cv", folds = 3L)
rr <- resample(task_iris, learner, resampling)
rr$score(measure)