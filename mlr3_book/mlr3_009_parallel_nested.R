
# run parallel computations -----------------------------------------------

library(mlr3learners)
library(mlr3tuning)

learner = lrn("classif.rpart", 
              minsplit = to_tune(2, 128, logscale = TRUE)
              )

at = auto_tuner(
  method = tnr("random_search"),
  learner = learner,
  resampling = rsmp("cv", folds = 2), # inner loop 
  measure = msr("classif.ce"),
  term_evals = 20
)

# resampling
rr= resample(
  task = tsk("penguins"),
  learner = at,
  resampling = rsmp("cv", folds = 5) # outer loop 
)

# check the result
rr$score()



