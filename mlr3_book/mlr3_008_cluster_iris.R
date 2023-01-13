
# clustering examples -----------------------------------------------------
# https://mlr3book.mlr-org.com/special.html

library(mlr3)
library(mlr3cluster)
library(mlr3viz)
set.seed(1)

design = benchmark_grid(
  tasks = TaskClust$new("iris", iris[-5]),
  learners = list(
    lrn("clust.kmeans", centers = 3L),
    lrn("clust.pam", k = 2L),
    lrn("clust.agnes")),
  resamplings = rsmp("insample")
)

print(design)

# run the benchmark
bmr = benchmark(design)

# model evaluation and measurement
measures = list(msr("clust.wss"), msr("clust.silhouette"))
bmr$aggregate(measures)

# visual inspection
task2 = TaskClust$new("iris", iris[-5])
learner2 = lrn("clust.kmeans")
learner2$train(task2)
prediction2 = learner2$predict(task2)

# display the results
autoplot(prediction2, task2, type = "pca")

# with ellipse grouping
autoplot(prediction2, 
         task2, 
         type = "pca", 
         frame = TRUE, 
         frame.type = "norm")


