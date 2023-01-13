
# Hyperparameter Optimization ---------------------------------------------

# https://mlr3book.mlr-org.com/optimization.html
library(mlr3)
library(mlr3verse)
library(mlr3viz)

# load the data
task = tsk("penguins")


# Nested Resampling -------------------------------------------------------
# set the learner space
learner = lrn("classif.rpart",
              cp        = to_tune(1e-04, 1e-1, logscale = TRUE),
              minsplit  = to_tune(2, 128, logscale = TRUE)
)

# auto tuner
at = auto_tuner(
  method = tnr("random_search"),
  learner = learner,
  resampling = rsmp("cv", folds = 4),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals= 5),
)     

# data not seen 1/3 not used in autotuner
outer_resampling = rsmp("cv", folds = 3)

# run the iterators
rr = resample(task, at, outer_resampling, store_models = TRUE)

# extract optimized parameters
extract_inner_tuning_results(rr)

# check the score or results
rr$score()
rr$aggregate()
