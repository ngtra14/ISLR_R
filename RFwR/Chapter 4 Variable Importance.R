# 4.5.1 An Illustration by Simulation in Regression -----------------------

library(mlbench)
library(randomForest)
fried1Simu <- mlbench.friedman1(n = 500)
fried1Data <- data.frame(fried1Simu$x, y = fried1Simu$y)
fried1RFimp <- randomForest(y ~ ., fried1Data, importance = TRUE)
varImpPlot(fried1RFimp, type = 1, scale = FALSE,
             main = "Variable importance")

# partial effect
partialPlot(fried1RFimp, fried1Data, x.var = "X1", main = "X1")

# 4.5.2 Predicting Ozone Concentration
library("randomForest")
data("Ozone", package = "mlbench")
OzRFDefImp <- randomForest(V4 ~ ., Ozone, na.action = na.omit,
                             importance = TRUE)
varImpPlot(OzRFDefImp, type = 1, scale = FALSE,
           main = "Variable importance")

# 4.5.3 Analyzing Genomic Data
library(randomForest)
data("vac18", package = "mixOmics")
geneExpr <- vac18$genes
stimu <- vac18$stimulation

vacRFDefImp <- randomForest(x = geneExpr, y = stimu,
                    mtry = ncol(geneExpr)/3, importance = TRUE)
varImpPlot(vacRFDefImp, type = 1, scale = FALSE, cex = 0.8)

vacImp <- vacRFDefImp$importance[, nlevels(stimu) + 1]
plot(sort(vacImp, decreasing = TRUE), type = "l",
      xlab = "Variables", ylab = "Variable importance")