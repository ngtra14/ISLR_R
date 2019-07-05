# A Short Introduction to the caret Package
# https://cran.r-project.org/web/packages/caret/vignettes/caret.html
library(caret)
library(mlbench)
data(Sonar)
head(Sonar)
str(Sonar)

set.seed(107)
# split data
inTrain <- createDataPartition(y=Sonar$Class,
                               p = 0.75,
                               list = FALSE)

str(inTrain)
head(inTrain)
training <- Sonar[inTrain, ]
testing <- Sonar[-inTrain, ]
nrow(training)
nrow(testing)

# tune a model

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
plsFit <- train(Class ~., 
                data = training,
                method = "pls",
                tuneLength = 15,
                trControl = ctrl,
                metric = "ROC",
                preProc = c("center", "scale"))

plsFit

# plot results
plot(plsFit)

# predict on new data
plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)

plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsFit)

# results comparison
confusionMatrix(data = plsClasses, testing$Class)

# grid search
rdaGrid <- data.frame(gamma=(0:4)/4, lambda=3/4)
set.seed(123)
rdaFit <- train(Class ~.,
                data = training,
                method = "rda",
                tuneGrid = rdaGrid,
                trControl = ctrl,
                metric = "ROC")

rdaFit

rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)

# resample
resamps <- resamples(list(pls=plsFit, rda=rdaFit))
summary(resamps)

# plots
diffs <- diff(resamps)
summary(diffs)
xyplot(resamps, what = "BlandAltman")
