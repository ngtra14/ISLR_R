# Chapter 3 Random Forests ------------------------------------------------

# 3.3 The randomForest Package
data("spam", package = "kernlab")
set.seed(9146301)
levels(spam$type) <- c("ok", "spam")
yTable <- table(spam$type)
indApp <- c(sample(1:yTable[2], yTable[2]/2),
            sample((yTable[2] + 1):nrow(spam), yTable[1]/2))
spamApp <- spam[indApp, ]
spamTest <- spam[-indApp, ]
head(spam)
RFDef <- randomForest(type ~. , data = spamApp)
RFDef

# RFDef <- randomForest(spamApp[, -58], spamApp[, 58])
errTestRFDef <- mean(predict(RFDef, spamTest) != spamTest$type)
errEmpRFDef <- mean(predict(RFDef, spamApp) != spamApp$type)

# OOB error
plot(RFDef)

# do.trace option
RFDoTrace <- randomForest(type~ ., data = spamApp, 
                          ntree = 250, do.trace = 25)
# optimal value of mtry
nbvars <- 1:(ncol(spamApp) - 1)
oobsMtry <- sapply(nbvars, function(nbv) {
  RF <- randomForest(type ~., spamApp, ntree = 250, mtry = nbv)
  return(RF$err.rate[RF$ntree, "OOB"])
})

plot(oobsMtry, type = "l")

mean(replicate(n = 25, 
               randomForest(type ~., spamApp, 
                            ntree = 250)$err.rate[250, "OOB"]))

# two-leaf trees (also called stumps)
bagStump <- randomForest(type ~ ., spamApp, ntree = 100,
                         mtry = ncol(spamApp) - 1, maxnodes = 2)

bagStumpbestvar <- table(bagStump$forest$bestvar[1, ])
names(bagStumpbestvar) <- colnames(spamApp)[
  as.numeric(names(bagStumpbestvar))]
sort(bagStumpbestvar, decreasing = TRUE)

# an RF made of 100 stumps
RFStump <- randomForest(type ~ ., spamApp, ntree = 100,
                       maxnodes = 2)
RFStumpbestvar <- table(RFStump$forest$bestvar[1, ])
names(RFStumpbestvar) <- colnames(spamApp)[
  as.numeric(names(RFStumpbestvar))]
sort(RFStumpbestvar, decreasing = TRUE)

# 3.6.1 Predicting Ozone Concentration
library("randomForest")
data("Ozone", package = "mlbench")
OzRFDef <- randomForest(V4 ~., Ozone, na.action = na.omit)
OzRFDef
plot(OzRFDef)

# stratification
bins <- c(0, 10, 20, 40)
V4bin <- cut(Ozone$V4, bins, include.lowest = TRUE, right = FALSE)
OzoneBin <- data.frame(Ozone, V4bin)
OzRFDefStrat <- randomForest(V4 ~. - V9 - V4bin, OzoneBin, 
                             strata = V4bin, 
                             sampsize = 200, 
                             na.action = na.omit)
OzRFDefStrat

# 3.6.2 Analyzing Genomic Data
library(randomForest)
data("vac18", package = "mixOmics")
geneExpr <- vac18$genes
stimu <- vac18$stimulation

VacRFpsur3 <- randomForest(x = geneExpr, y = stimu,
                           mtry = ncol(geneExpr)/3)
VacRFpsur3
plot(VacRFpsur3)

# 3.6.3 Analyzing Dust Pollution
library(randomForest)
data("jus", package = "VSURF")
jusComp <- na.omit(jus)

jusRF <- randomForest(PM10 ~., data = jusComp)

# marginal effect of NO variable
partialPlot(jusRF, pred.data = jusComp, x.var = "NO",
            main = "Marginal effect - NO")
