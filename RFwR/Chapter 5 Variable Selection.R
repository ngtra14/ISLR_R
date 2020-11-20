# Chapter 5 Variable Selection --------------------------------------------
# 5.4 The VSURF Package
library(VSURF)
data("toys")

str(toys)

set.seed(3101318)
vsurfToys <- VSURF(toys$x, toys$y, mtry = 100)
summary(vsurfToys)
plot(vsurfToys)

# construction of forests and the ranking and elimination steps
set.seed(3101318)
vsurfThresToys <- VSURF_thres(toys$x, toys$y, mtry = 100)

vsurfThresToys$varselect.thres

plot(vsurfToys, step = "thres", imp.mean = FALSE,
     ylim = c(0, 2e-04))

vsurfInterpToys <- VSURF_interp(toys$x, toys$y,
                        vars = vsurfThresToys$varselect.thres)
vsurfInterpToys$varselect.interp

vsurfPredToys <- VSURF_pred(toys$x, toys$y,
                      err.interp = vsurfInterpToys$err.interp,
          varselect.interp = vsurfInterpToys$varselect.interp)

vsurfPredToys$varselect.pred

# 5.6.1 Predicting Ozone Concentration
library(VSURF)
data("Ozone", package = "mlbench")
set.seed(303601)
OzVSURF <- VSURF(V4 ~., data = Ozone, na.action = na.omit)
summary(OzVSURF)

plot(OzVSURF, var.names = TRUE)

number <- c(1:3, 5:13)
number[OzVSURF$varselect.thres]
number[OzVSURF$varselect.interp]
number[OzVSURF$varselect.pred]

# 5.6.2 Analyzing Genomic Data
library(randomForest)
data("vac18", package = "mixOmics")
geneExpr <- vac18$genes
stimu <- vac18$stimulation

set.seed(481933)
vacVSURF <- VSURF(x = geneExpr, y = stimu)
summary(vacVSURF)
plot(vacVSURF)

probeSelPred <- colnames(geneExpr)[vacVSURF$varselect.pred]
probeSelPred

# set.seed(627408, kind = "L'Ecuyer-CMRG")
# vacVSURFpara <- VSURF(x = geneExpr, y = stimu, parallel = TRUE,
#                       ncores = 3, clusterType = "FORK")
# summary(vacVSURFpara)