# 22 Feature Selection using Simulated Annealing --------------------------
# MAY NEED PARALLEL TO SPEED UP
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

sa_ctrl <- safsControl(functions = rfSA,
                       method = "repeatedcv",
                       repeats = 5,
                       improve = 50)

set.seed(10)
rf_sa <- safs(x = x, y = y,
              iters = 25,
              safsControl = sa_ctrl)
rf_sa
plot(rf_sa) + theme_bw()

