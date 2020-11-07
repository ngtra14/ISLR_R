# 3 Pre-Processing --------------------------------------------------------
# https://topepo.github.io/caret/pre-processing.html
# 3.1 Creating Dummy Variables
library(earth)
data(etitanic)
head(model.matrix(survived ~ ., data = etitanic))

# Using dummyVars
dummies <- dummyVars(survived ~ ., data = etitanic)
head(predict(dummies, newdata = etitanic))

# 3.2 Zero- and Near Zero-Variance Predictors
data(mdrr)
data.frame(table(mdrrDescr$nR11))

nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]
dim(mdrrDescr)

nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]
dim(filteredDescr)

# 3.3 Identifying Correlated Predictors
descrCor <-  cor(filteredDescr)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)

descrCor <- cor(filteredDescr)
summary(descrCor[upper.tri(descrCor)])

highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])

# 3.4 Linear Dependencies
ltfrDesign <- matrix(0, nrow=6, ncol=6)
ltfrDesign[,1] <- c(1, 1, 1, 1, 1, 1)
ltfrDesign[,2] <- c(1, 1, 1, 0, 0, 0)
ltfrDesign[,3] <- c(0, 0, 0, 1, 1, 1)
ltfrDesign[,4] <- c(1, 0, 0, 1, 0, 0)
ltfrDesign[,5] <- c(0, 1, 0, 0, 1, 0)
ltfrDesign[,6] <- c(0, 0, 1, 0, 0, 1)

comboInfo <- findLinearCombos(ltfrDesign)
comboInfo
ltfrDesign[, -comboInfo$remove]

# 3.5 The preProcess Function
# 3.6 Centering and Scaling
set.seed(96)
inTrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)

training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, test)

# 3.7 Imputation
# 3.8 Transforming Predictors
plotSubset <- data.frame(scale(mdrrDescr[, c("nC", "X4v")])) 
xyplot(nC ~ X4v,
       data = plotSubset,
       groups = mdrrClass, 
       auto.key = list(columns = 2))  

# After the spatial sign:
transformed <- spatialSign(plotSubset)
transformed <- as.data.frame(transformed)
xyplot(nC ~ X4v, 
       data = transformed, 
       groups = mdrrClass, 
       auto.key = list(columns = 2)) 

# Box-Cox transformation 
preProcValues2 <- preProcess(training, method = "BoxCox")
trainBC <- predict(preProcValues2, training)
testBC <- predict(preProcValues2, test)
preProcValues2

# 3.9 Putting It All Together
library(AppliedPredictiveModeling)
data(schedulingData)
str(schedulingData)

pp_hpc <- preProcess(schedulingData[, -8], 
                     method = c("center", "scale", "YeoJohnson"))
pp_hpc

transformed <- predict(pp_hpc, newdata = schedulingData[, -8])
head(transformed)
mean(schedulingData$NumPending == 0)

pp_no_nzv <- preProcess(schedulingData[, -8], 
                        method = c("center", "scale", "YeoJohnson", "nzv"))
pp_no_nzv
predict(pp_no_nzv, newdata = schedulingData[1:6, -8])

# 3.10 Class Distance Calculations
centroids <- classDist(trainBC, trainMDRR)
distances <- predict(centroids, testBC)
distances <- as.data.frame(distances)
head(distances)

xyplot(dist.Active ~ dist.Inactive,
       data = distances, 
       groups = testMDRR, 
       auto.key = list(columns = 2))
