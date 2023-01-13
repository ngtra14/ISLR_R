
# mlr3_001_intro.R --------------------------------------------------------

library(mlbench)
library(ggplot2)

data("PimaIndiansDiabetes", package = "mlbench")


library(mlr3)
library(mlr3verse)
library(mlr3viz)
task_diabetes <- as_task_classif(PimaIndiansDiabetes, 
                                 target = "diabetes",
                                 positive = "pos")

# create a list of models
design = benchmark_grid(
  tasks = tsks(c("sonar", "pima")),
  learners = lrns(c("classif.rpart", "classif.featureless"),
                  predict_type = "prob", 
                  predict_sets = c("train", "test")), 
  resamplings = rsmps("cv", folds = 3)
  
)

print(design)

bmr = benchmark(design)

measures = list(
  msr("classif.auc", predict_sets = "train", id = "auc_train"),
  msr("classif.auc", id = "auc_test")
)

tab = bmr$aggregate(measures)
print(tab[, .(task_id, learner_id, auc_train, auc_test)])

# plot benchmark
autoplot(bmr) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bmr_small = bmr$clone(deep = TRUE)$filter(task_id = "pima")
autoplot(bmr_small, type = "roc")  

# statistical tests
library("mlr3benchmark")

bma = as.BenchmarkAggr(bmr, measures = msr("classif.auc"))
bma$friedman_posthoc()
autoplot(bma, type = "mean")
