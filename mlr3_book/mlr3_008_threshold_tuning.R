
# 8.4 Cost-Sensitive Classification ---------------------------------------
# https://mlr3book.mlr-org.com/special.html

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

learner = lrn("classif.log_reg", predict_type = "prob")
rr = resample(task, learner, rsmp("cv"))

p = rr$prediction()
print(p)

# change threshold with a helper function
with_threshold = function(p, th) {
  p$set_threshold(th)
  list(confusion = p$confusion, costs = p$score(measures = cost_measure))
}

# plot the curve
autoplot(p, type = "threshold", measure = cost_measure)

with_threshold(p, 0.5)
with_threshold(p, 0.75)

# locate optimal point for threshold
f = function(th) {
  with_threshold(p, th)$costs
}

best_point = optimize(f, c(0.5, 1))
print(best_point)
