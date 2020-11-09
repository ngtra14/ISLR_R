# 5 Model Training and Tuning ---------------------------------------------

# https://topepo.github.io/caret/model-training-and-tuning.html

# 5.1 Model Training and Parameter Tuning

# 5.2 An Example
library(mlbench)
data(Sonar)
str(Sonar[, 1:10])

library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

# 5.3 Basic Parameter Tuning
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(825)
gbmFit1 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

# 5.4 Notes on Reproducibility
# 5.5 Customizing the Tuning Process
# 5.5.1 Pre-Processing Options
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2

# 5.5.3 Plotting the Resampling Profile
trellis.par.set(caretTheme())
plot(gbmFit2)  

trellis.par.set(caretTheme())
plot(gbmFit2, metric = "Kappa")

trellis.par.set(caretTheme())
plot(gbmFit2, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))

ggplot(gbmFit2)  

# 5.5.4 The trainControl Function
# 5.5.5 Alternate Performance Metrics

head(twoClassSummary)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

set.seed(825)
gbmFit3 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = "ROC")
gbmFit3

# 5.6 Choosing the Final Model

whichTwoPct <- tolerance(gbmFit3$results, metric = "ROC", 
                         tol = 2, maximize = TRUE)  
cat("best model within 2 pct of best:\n")
gbmFit3$results[whichTwoPct,1:6]

# 5.7 Extracting Predictions and Class Probabilities
predict(gbmFit3, newdata = head(testing))

predict(gbmFit3, newdata = head(testing), type = "prob")

# 5.8 Exploring and Comparing Resampling Distributions
# 5.8.1 Within-Model
trellis.par.set(caretTheme())
densityplot(gbmFit3, pch = "|")

# 5.8.2 Between-Models
set.seed(825)
svmFit <- train(Class ~ ., data = training, 
                method = "svmRadial", 
                trControl = fitControl, 
                preProc = c("center", "scale"),
                tuneLength = 8,
                metric = "ROC")
svmFit  

# a regularized discriminant analysis model was fit
set.seed(825)
rdaFit <- train(Class ~ ., data = training, 
                method = "rda", 
                trControl = fitControl, 
                tuneLength = 4,
                metric = "ROC")
rdaFit    

resamps <- resamples(list(GBM = gbmFit3,
                          SVM = svmFit,
                          RDA = rdaFit))
resamps

summary(resamps)

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))

trellis.par.set(caretTheme())
dotplot(resamps, metric = "ROC")

trellis.par.set(theme1)
xyplot(resamps, what = "BlandAltman")

splom(resamps)

difValues <- diff(resamps)
difValues
summary(difValues)

trellis.par.set(theme1)
bwplot(difValues, layout = c(3, 1))

trellis.par.set(caretTheme())
dotplot(difValues)

# 5.9 Fitting Models Without Parameter Tuning
fitControl <- trainControl(method = "none", classProbs = TRUE)

set.seed(825)
gbmFit4 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Only a single model can be passed to the
                 ## function when no resampling is used:
                 tuneGrid = data.frame(interaction.depth = 4,
                                       n.trees = 100,
                                       shrinkage = .1,
                                       n.minobsinnode = 20),
                 metric = "ROC")
gbmFit4
predict(gbmFit4, newdata = head(testing))
predict(gbmFit4, newdata = head(testing), type = "prob")