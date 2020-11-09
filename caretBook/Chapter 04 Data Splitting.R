# 4 Data Splitting --------------------------------------------------------

# 4.1 Simple Splitting Based on the Outcome
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(iris$Species, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

irisTrain <- iris[ trainIndex,]
irisTest  <- iris[-trainIndex,]

# 4.2 Splitting Based on the Predictors
library(mlbench)
data(BostonHousing)

testing <- scale(BostonHousing[, c("age", "nox")])
set.seed(5)
## A random sample of 5 data points
startSet <- sample(1:dim(testing)[1], 5)
samplePool <- testing[-startSet,]
start <- testing[startSet,]
newSamp <- maxDissim(start, samplePool, n = 20)
head(newSamp)

# 4.3 Data Splitting for Time Series
# 4.4 Simple Splitting with Important Groups
set.seed(3527)
subjects <- sample(1:6, size = 50, replace = TRUE) # interesting !
table(subjects)

folds <- groupKFold(subjects, k = 3) 
folds
