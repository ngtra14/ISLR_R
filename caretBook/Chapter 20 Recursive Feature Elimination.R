# 20 Recursive Feature Elimination ----------------------------------------
# 20.3 Recursive Feature Elimination via caret
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)

#  simulation
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

# Of the 50 predictors, there are 45 pure noise variables: 5 are uniform on
# 0,1 and 40 are random univariate standard normals. The predictors are 
# centered and scaled:

normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)
subsets <- c(1:5, 10, 15, 20, 25)

set.seed(10)
ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
lmProfile <- rfe(x, y,
                 sizes = subsets,
                 rfeControl = ctrl)
lmProfile
predictors(lmProfile)
lmProfile$fit
head(lmProfile$resample)

trellis.par.set(caretTheme())
plot(lmProfile, type = c("g", "o"))

# 20.6 The Example
rfRFE <-  list(summary = defaultSummary,
               fit = function(x, y, first, last, ...){
                 library(randomForest)
                 randomForest(x, y, importance = first, ...)
               },
               pred = function(object, x)  predict(object, x),
               rank = function(object, x, y) {
                 vimp <- varImp(object)
                 vimp <- vimp[order(vimp$Overall,decreasing = TRUE),,drop = FALSE]
                 vimp$var <- rownames(vimp)                  
                 vimp
               },
               selectSize = pickSizeBest,
               selectVar = pickVars)

example <- data.frame(RMSE = c(3.215, 2.819, 2.414, 2.144, 
                               2.014, 1.997, 2.025, 1.987, 
                               1.971, 2.055, 1.935, 1.999, 
                               2.047, 2.002, 1.895, 2.018),
                      Variables = 1:16)
## Find the row with the absolute smallest RMSE
smallest <- pickSizeBest(example, metric = "RMSE", maximize = FALSE)
smallest

## Now one that is within 10% of the smallest
within10Pct <- pickSizeTolerance(example, metric = "RMSE", 
                                 tol = 10, maximize = FALSE)
within10Pct

minRMSE <- min(example$RMSE)
example$Tolerance <- (example$RMSE - minRMSE)/minRMSE * 100   

## Plot the profile and the subsets selected using the 
## two different criteria

par(mfrow = c(2, 1), mar = c(3, 4, 1, 2))

plot(example$Variables[-c(smallest, within10Pct)], 
     example$RMSE[-c(smallest, within10Pct)],
     ylim = extendrange(example$RMSE),
     ylab = "RMSE", xlab = "Variables")

points(example$Variables[smallest], 
       example$RMSE[smallest], pch = 16, cex= 1.3)

points(example$Variables[within10Pct], 
       example$RMSE[within10Pct], pch = 17, cex= 1.3)

with(example, plot(Variables, Tolerance))
abline(h = 10, lty = 2, col = "darkgrey")

# 20.5.6 The selectVar Function
rfRFE$selectVar

# 20.6 The Example
ctrl$functions <- rfRFE
ctrl$returnResamp <- "all"
set.seed(10)
rfProfile <- rfe(x, y, sizes = subsets, rfeControl = ctrl)
rfProfile

trellis.par.set(caretTheme())
dev.off()
plot1 <- plot(rfProfile, type = c("g", "o"))
plot2 <- plot(rfProfile, type = c("g", "o"), metric = "Rsquared")
print(plot1, split=c(1,1,1,2), more=TRUE)
print(plot2, split=c(1,2,1,2))

plot1 <- xyplot(rfProfile, 
                type = c("g", "p", "smooth"), 
                ylab = "RMSE CV Estimates")
plot2 <- densityplot(rfProfile, 
                     subset = Variables < 5, 
                     adjust = 1.25, 
                     as.table = TRUE, 
                     xlab = "RMSE CV Estimates", 
                     pch = "|")
print(plot1, split=c(1,1,1,2), more=TRUE)
print(plot2, split=c(1,2,1,2))