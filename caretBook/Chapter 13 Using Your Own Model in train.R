# 13 Using Your Own Model in train ----------------------------------------

# 13.1 Introduction
library(caret)
ldaModelInfo <- getModelInfo(model = "lda", regex = FALSE)[[1]]
## Model components
names(ldaModelInfo)

# 13.2 Illustrative Example 1: SVMs with Laplacian Kernels

# 13.3 Model Components
lpSVM <- list(type = "Classification",
              library = "kernlab",
              loop = NULL) 

# 13.3.1 The parameters Element
prm <- data.frame(parameter = c("C", "sigma"),
                  class = rep("numeric", 2),
                  label = c("Cost", "Sigma"))
lpSVM$parameters <- prm

# 13.3.2 The grid Element
svmGrid <- function(x, y, len = NULL, search = "grid") {
  library(kernlab)
  ## This produces low, middle and high values for sigma 
  ## (i.e. a vector with 3 elements). 
  sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)  
  ## To use grid search:
  if(search == "grid") {
    out <- expand.grid(sigma = mean(as.vector(sigmas[-2])),
                       C = 2 ^((1:len) - 3))
  } else {
    ## For random search, define ranges for the parameters then
    ## generate random values for them
    rng <- extendrange(log(sigmas), f = .75)
    out <- data.frame(sigma = exp(runif(len, min = rng[1], max = rng[2])),
                      C = 2^runif(len, min = -5, max = 8))
  }
  out
}
lpSVM$grid <- svmGrid

# 13.3.3 The fit Element
svmFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) { 
  kernlab::ksvm(
    x = as.matrix(x), y = y,
    kernel = "rbfdot",
    kpar = list(sigma = param$sigma),
    C = param$C,
    prob.model = classProbs,
    ...
  )
}

lpSVM$fit <- svmFit

# 13.3.4 The predict Element
svmPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  kernlab::predict(modelFit, newdata)
lpSVM$predict <- svmPred

# 13.3.5 The prob Element
svmProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  kernlab::predict(modelFit, newdata, type = "probabilities")
lpSVM$prob <- svmProb

# 13.4 The sort Element
svmSort <- function(x) x[order(x$C),]
lpSVM$sort <- svmSort

# 13.4.1 The levels Element
lpSVM$levels <- function(x) kernlab::lev(x)
function(x) levels(x@data@get("response")[,1])

# fit our model
library(mlbench)
data(Sonar)

library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

fitControl <- trainControl(method = "repeatedcv",
                           ## 10-fold CV...
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

set.seed(825)
Laplacian <- train(Class ~ ., data = training, 
                   method = lpSVM, 
                   preProc = c("center", "scale"),
                   tuneLength = 8,
                   trControl = fitControl)
Laplacian
ggplot(Laplacian) + scale_x_log10()
