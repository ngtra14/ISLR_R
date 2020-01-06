# Model optimization

# 3.1 Hyperparameter Tuning -----------------------------------------------

library(mlr3)
task = tsk("pima")
print(task)
print(as.data.table(task))

learner = lrn("classif.rpart")
learner$param_set

library(paradox)
tune_ps = ParamSet$new(list(
  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  ParamInt$new("minsplit", lower = 1, upper = 10)
))
tune_ps
hout = rsmp("holdout")
measure = msr("classif.ce")

library(mlr3tuning)
evals20 = term("evals", n_evals = 20)

instance = TuningInstance$new(
  task = task,
  learner = learner,
  resampling = hout,
  measures = measure,
  param_set = tune_ps,
  terminator = evals20
)

print(instance)

# 3.1.2 The Tuner Class
tuner = tnr("grid_search", resolution = 5)
result = tuner$tune(instance)
print(result)
instance$archive(unnest = "params")[, c("cp", "minsplit", "classif.ce")]

learner$param_set$values = instance$result$params
learner$train(task)

# 3.1.4 Automating the Tuning
library(paradox)
library(mlr3tuning)

learner = lrn("classif.rpart")
resampling = rsmp("holdout")
measures = msr("classif.ce")
tune_ps = ParamSet$new(list(
  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  ParamInt$new("minsplit", lower = 1, upper = 10)
))
terminator = term("evals", n_evals = 10)
tuner = tnr("random_search")

at = AutoTuner$new(
  learner = learner,
  resampling = resampling,
  measures = measures,
  tune_ps = tune_ps,
  terminator = terminator,
  tuner = tuner
)
at
