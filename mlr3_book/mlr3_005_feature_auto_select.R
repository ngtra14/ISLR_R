
# feature auto selection --------------------------------------------------

library(mlr3)
library(mlr3verse)
library(mlr3fselect)

task = tsk("penguins")

learner = lrn("classif.rpart")

at = auto_fselector(
  method = fs("random_search"),
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.acc"),
  terminator = trm("evals", n_evals = 2)
)


grid = benchmark_grid(
  task = tsk("penguins"),
  learner = list(at, lrn("classif.rpart")),
  resampling = rsmp("cv", folds = 3)
)

bmr = benchmark(grid)

aggr <- bmr$aggregate(msrs(c("classif.acc", "time_train")))
as.data.table(aggr)[, .(learner_id, classif.acc, time_train)]

# surprisingly, it did not converge