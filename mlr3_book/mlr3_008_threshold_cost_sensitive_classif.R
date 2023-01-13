
# 8.4 Cost-Sensitive Classification ---------------------------------------
library(mlr3)
library(mlr3verse)
library(mlr3benchmark)

task = tsk("german_credit")
table(task$truth())

#  cost-matrix 
costs = matrix(c(-0.35, 0, 1, 0), nrow = 2)
dimnames(costs) = list(response = c("good", "bad"), truth = c("good", "bad"))
print(costs)

# two extreme cases
# nobody:
(700 * costs[2, 1] + 300 * costs[2, 2]) / 1000

# everybody
(700 * costs[1, 1] + 300 * costs[1, 2]) / 1000

# average profit * average loan * number of customers
0.055 * 20000 * 1000 # lost 1.1 million


# ordinary logistic regression --------------------------------------------

learner = lrn("classif.log_reg")
rr = resample(task, learner, rsmp("cv"))

confusion = rr$prediction()$confusion
print(confusion)

# calculate average cost
avg_costs = sum(confusion * costs) / 1000
print(avg_costs)

# costs or losses using logistic regression 
avg_costs * 20000 * 1000 # 1.09 million


# Cost-sensitive Measure --------------------------------------------------

cost_measure = msr("classif.costs", costs = costs)
print(cost_measure)

# compare log regression, dummy model against rf
learners = list(
  lrn("classif.log_reg"),
  lrn("classif.featureless"),
  lrn("classif.ranger")
)

cv10 = rsmp("cv", folds = 10)
bmr = benchmark(benchmark_grid(task, learners, cv10))

autoplot(bmr, measure = cost_measure) +
  ggplot2::geom_hline(yintercept = 0, colour = "red")
