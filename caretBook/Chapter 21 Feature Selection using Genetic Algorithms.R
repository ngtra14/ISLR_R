# 21 Feature Selection using Genetic Algorithms ---------------------------
# 21.4 Genetic Algorithm Example
# SOMEHOW, NOT ABLE TO CONVERGE...
library(caret)
library(mlbench)
n <- 100
p <- 40
sigma <- 1
set.seed(1)
sim <- mlbench.friedman1(n, sd = sigma)
colnames(sim$x) <- c(paste("real", 1:5, sep = ""),
                     paste("bogus", 1:5, sep = ""))
bogus <- matrix(rnorm(n * p), nrow = n)
colnames(bogus) <- paste("bogus", 5+(1:ncol(bogus)), sep = "")
x <- cbind(sim$x, bogus)
y <- sim$y
normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)

ga_ctrl <- gafsControl(functions = rfGA,
                       method = "repeatedcv",
                       repeats = 5)

## Use the same random number seed as the RFE process
## so that the same CV folds are used for the external
## resampling. 
set.seed(10)
rf_ga <- gafs(x = x, y = y,
              iters = 10,
              gafsControl = ga_ctrl)
rf_ga

plot(rf_ga) + theme_bw()

# 21.6 The Example Revisited
library(desirability)
rfGA2 <- rfGA
rfGA2$fitness_intern <- function (object, x, y, maximize, p) {
  RMSE <- rfStats(object)[1]
  d_RMSE <- dMin(0, 4)
  d_Size <- dMin(1, p, 2)
  overall <- dOverall(d_RMSE, d_Size)
  D <- predict(overall, data.frame(RMSE, ncol(x)))
  c(D = D, RMSE = as.vector(RMSE))
}
ga_ctrl_d <- gafsControl(functions = rfGA2,
                         method = "repeatedcv",
                         repeats = 5,
                         metric = c(internal = "D", external = "RMSE"),
                         maximize = c(internal = TRUE, external = FALSE))

set.seed(10)
rf_ga_d <- gafs(x = x, y = y,
                iters = 1,
                gafsControl = ga_ctrl_d)

rf_ga_d
plot(rf_ga_d) + theme_bw()
