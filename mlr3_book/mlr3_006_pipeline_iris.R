
# mlr3 pipepline example --------------------------------------------------
# https://cran.r-project.org/web/packages/mlr3pipelines/mlr3pipelines.pdf

# Graph class intro -------------------------------------------------------

library(mlr3)
library(mlr3pipelines)
g = Graph$new()$
  add_pipeop(PipeOpScale$new(id = "scale"))$
  add_pipeop(PipeOpPCA$new(id = "pca"))$
  add_edge("scale", "pca")

g$input
g$output

task = tsk("iris")
trained = g$train(task)
trained[[1]]$data()

task$filter(1:10)
predicted = g$predict(task)
predicted[[1]]$data()

str(predicted)


# include Graph as a learner ----------------------------------------------


# Unite Binary Classification Tasks ---------------------------------------

task = tsk("iris")
gr = po("ovrsplit") %>>% lrn("classif.rpart") %>>% po("ovrunite")  
gr$train(task)
gr$predict(task)
gr$pipeops$classif.rpart$learner$predict_type = "prob"
gr$predict(task)


# Principle Component Analysis --------------------------------------------

library(mlr3)
task = tsk("iris")
pop = po("pca")
task$data()
pop$train(list(task))[[1]]$data()
pop$state


# Wrap another PipeOp or Graph as a Hyperparameter ------------------------

library(mlr3)
library(mlr3learners)
set.seed(1234)
task = tsk("iris")

# preprocessing and learner 
# - no preprocessing and use kknn
g = po("proxy", id = "prepoc", param_vals = list(content = po("nop"))) %>>%
  po("proxy", id = "learner", param_vals = list(content = lrn("classif.kknn")))

rr_kknn = resample(task, learner = GraphLearner$new(g), resampling = rsmp("cv", folds = 3))
rr_kknn$aggregate(msr("classif.ce"))

# use pca for preprocessing + rpart as learner
g$param_set$values$prepoc.content = po("pca")
g$param_set$values$learner.content = lrn("classif.rpart")
rr_pca_rpart = resample(task, learner = GraphLearner$new(g),
                        resampling = rsmp("cv", folds = 3))
rr_pca_rpart$aggregate(msr("classif.ce"))



# Generate a Randomized Response Prediction -------------------------------
task2 = tsk("mtcars")
g2 = LearnerRegrLM$new() %>>% PipeOpRandomResponse$new() 
g2$train(task2)
g2$pipeops$regr.lm$learner$predict_type = "se"
set.seed(2906)
g2$predict(task2)


# Weighted Prediction Averaging -------------------------------------------

library(mlr3)
# bagging 
gr = ppl("greplicate",
         po("subsample") %>>%
         po("learner", lrn("classif.rpart")),
         n = 5 
         ) %>>% 
         po("classifavg")

resample(tsk("iris"), GraphLearner$new(gr), rsmp("holdout"))


# Center and Scale Numeric Features ---------------------------------------

library(mlr3)
task = tsk("iris")
pos = po("scale")
pos$train(list(task))[[1]]$data()
one_line_of_iris = task$filter(13)
one_line_of_iris$data()
pos$predict(list(one_line_of_iris))[[1]]$data()
