# Chapter 2 Basics
library(mlr3)
data("mtcars", package = "datasets")
data <- mtcars[, 1:3]
str(data)


# 2.2.2 task creation -----------------------------------------------------

task_mtcars <- TaskRegr$new(id = "cars", backend = data, target = "mpg")
print(task_mtcars)

# library(mlr3viz)


# 2.2.3 predefined tasks --------------------------------------------------

mlr_tasks

library(data.table)
as.data.table(mlr_tasks)

task_iris <- mlr_tasks$get("iris")
print(task_iris)

tsk("iris")

# retrieving data
task_iris$nrow
task_iris$ncol

task_iris$data()

task_iris$data(rows = c(1, 51, 101))

task_mtcars = tsk("mtcars")
head(task_mtcars$row_ids)

task_mtcars$data(rows = "Datsun 710")
task_iris$data(rows = c(1, 51, 101), cols = "Species")

summary(as.data.table(task_iris))

# rows andd columns
print(task_mtcars$col_roles)

data = as.data.table(mtcars[, 1:3], keep.rownames = TRUE)
task = TaskRegr$new(id = "cars", backend = data, target = "mpg")
task$row_ids


# 2.3 learners ------------------------------------------------------------

library(mlr3learners)
mlr_learners
learner = mlr_learners$get("classif.rpart")
print(learner)

learner$param_set
learner$param_set$values = list(cp = 0.01, xval = 0)
learner

learner$param_set$values = mlr3misc::insert_named(
  learner$param_set$values,
  list(cp = 0.02, minsplit = 2))  

learner
lrn("classif.rpart", id = "rp", cp = 0.01)    


# train and predict -------------------------------------------------------

task = tsk("sonar")
# head(sonar)
# head(as.data.table(sonar))
learner = lrn("classif.rpart")
train_set = sample(task$nrow, 0.8 * task$nrow)
test_set = setdiff(seq_len(task$nrow), train_set)

learner$model
learner$train(task, row_ids = train_set)

print(learner$model)

prediction = learner$predict(task, row_ids = test_set)
print(prediction)

head(as.data.table(prediction))

# confusion matrix
prediction$confusion

# change the predition type
learner$predict_type = "prob"
learner$train(task, row_ids = train_set)
prediction = learner$predict(task, row_ids = test_set)

head(as.data.table(prediction))

head(prediction$response)
head(prediction$prob)

# not on Cran
# remotes::install_github("mlr-org/mlr3viz")
library(mlr3viz)

task = tsk("sonar")
learner = lrn("classif.rpart", predict_type = "prob")
learner$train(task)
predition = learner$predict(task)
autoplot(prediction)
autoplot(prediction, type = "roc")

library(mlr3learners)
local({
  task = tsk("mtcars")
  learner = lrn("regr.lm")
  learner$train(task)
  prediction = learner$predict(task)
  autoplot(prediction)
})

# performance evaluation
mlr_measures
measure = msr("classif.acc")
prediction$score(measure)


# resampling --------------------------------------------------------------

task = tsk("iris")
learner = lrn("classif.rpart")
mlr_resamplings
resampling = rsmp("holdout")
print(resampling)

resampling$param_set$values = list(ratio = 0.8)
rsmp("holdout", ratio = 0.8)

# instantiation
reampling = rsmp("cv", folds = 3L)
resampling$instantiate(task)
resampling$iters

str(resampling$train_set(1))
str(resampling$test_set(1))

# execution
task = tsk("pima")
learner = lrn("classif.rpart", 
              maxdepth = 3,
              predict_type = "prob")

resampling = rsmp("cv", folds = 3L)
rr = resample(task, learner, resampling, store_models = TRUE)
print(rr)

rr$aggregate(msr("classif.ce")) # check performance

rr$score(msr("classif.ce"))

rr$warnings

lrn = rr$learners[[1]]
lrn$model

rr$prediction()

# custom resampling
# plotting
library(mlr3viz)
resampling = rsmp("custom")
resampling$instantiate(task,
                       train = list(c(1:100, 51:60, 101:110)),
                       test = list(c(11:20, 61:70, 111:120)))


autoplot(rr)
autoplot(rr, type = "roc") # not working
