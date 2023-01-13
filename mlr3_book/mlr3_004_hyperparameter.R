
# Hyperparameter Optimization ---------------------------------------------

# https://mlr3book.mlr-org.com/optimization.html
library(mlr3)
library(mlr3verse)
library(mlr3viz)

# load the data
task = tsk("penguins")

as.data.table(task)

# define learner
learner = lrn("classif.rpart")
learner$param_set

# two hyperparameters included
learner = lrn("classif.rpart",
              cp        = to_tune(1e-04, 1e-1, logscale = TRUE),
              minsplit  = to_tune(2, 128, logscale = TRUE)
)

# performance evaluation criteria
resampling = rsmp("cv", folds = 3)
measure = msr("classif.ce")

# number of iterations
terminator = trm("evals", n_evals = 20)
terminator

# define the space for searching
instance = ti(
  task = tsk("penguins"),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  terminator = trm("evals", n_evals = 20)
)

# set the turner
tuner = tnr("grid_search", resolution = 5, batch_size = 4)

# run the iterations
tuner$optimize(instance)

# inspect the results
head(as.data.table(instance$archive))

head(as.data.table(instance$archive, measures = msr("classif.acc")))

# view the results in 2D space/ surface
autoplot(instance, type = "surface")

# prepare the final model for predictions
learner$param_set$values = instance$result_learner_param_vals
learner$train(tsk("penguins"))          


# Nested Resampling -------------------------------------------------------

     
