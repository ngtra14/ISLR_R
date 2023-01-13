
# run parallel computations -----------------------------------------------

library(mlr3learners)
learner = lrn("classif.ranger")
learner$param_set$ids(tags = "threads")

# set number of CPUs
set_threads(learner, n = 2)

set_threads(learner)


# embarrassingly parallel -------------------------------------------------

future::plan("multisession")

# define a model to run
task = tsk("spam")
learner = lrn("classif.rpart")
resampling = rsmp("cv", folds = 5)
time = proc.time()[3]
resample(task, learner, resampling)
diff = proc.time()[3] - time 
