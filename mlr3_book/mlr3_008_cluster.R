
# clustering examples -----------------------------------------------------

library(mlr3)
library(mlr3cluster)
library(mlr3viz)
set.seed(1)

# construct a task
task = tsk("usarrests")
print(task) # check the structure

# check the data / task
autoplot(task)

# k-mean cluster
learner = lrn('clust.kmeans')
learner$train(task)
learner$model

# predictions on trained data
prediction = learner$predict(task)
autoplot(prediction, task)
