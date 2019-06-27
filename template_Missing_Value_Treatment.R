# template : Missing Value Treatment
# https://www.r-bloggers.com/missing-value-treatment/
# Data prep and pattern
# initialize the data
data ("BostonHousing", package="mlbench")
original <- BostonHousing  # backup original data

# Introduce missing values
set.seed(100)
BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"] <- NA

# Pattern of missing values
library(mice)
md.pattern(BostonHousing)  # pattern or missing values in data.

# 1. Deleting the observations
# Example
lm(medv ~ ptratio + rad, data=BostonHousing, na.action=na.omit)

# 2. Deleting the variable
# 3. Imputation with mean / median / mode
library(Hmisc)
impute(BostonHousing$ptratio, mean)  # replace with mean
impute(BostonHousing$ptratio, median)  # median
impute(BostonHousing$ptratio, 20)  # replace specific number

# or if you want to impute manually
BostonHousing$ptratio[is.na(BostonHousing$ptratio)] <- mean(BostonHousing$ptratio, 
                                                  na.rm = T)  # not run

# Lets compute the accuracy when it is imputed with mean
library(DMwR)
actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds <- rep(mean(BostonHousing$ptratio, na.rm=T), length(actuals))
regr.eval(actuals, predicteds)

# 4. Prediction
# 4.1. kNN Imputation
library(DMwR)
# perform knn imputation
knnOutput <- knnImputation(BostonHousing[, !names(BostonHousing) %in% "medv"])  
anyNA(knnOutput)

# Lets compute the accuracy.
actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds <- knnOutput[is.na(BostonHousing$ptratio), "ptratio"]
regr.eval(actuals, predicteds)

# 4.2 rpart
# The limitation with DMwR::knnImputation is that it sometimes may not be 
# appropriate to use when the missing value comes from a factor variable. 
library(rpart)
class_mod <- rpart(rad ~ . - medv, data=BostonHousing[!is.na(BostonHousing$rad), ], 
                   method="class", na.action=na.omit)  # since rad is a factor
anova_mod <- rpart(ptratio ~ . - medv, data=BostonHousing[!is.na(BostonHousing$ptratio), ], 
                   method="anova", na.action=na.omit)  # since ptratio is numeric.
rad_pred <- predict(class_mod, BostonHousing[is.na(BostonHousing$rad), ])
ptratio_pred <- predict(anova_mod, BostonHousing[is.na(BostonHousing$ptratio), ])

actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds <- ptratio_pred
regr.eval(actuals, predicteds)

actuals <- original$rad[is.na(BostonHousing$rad)]
predicteds <- as.numeric(colnames(rad_pred)[apply(rad_pred, 1, which.max)])
mean(actuals != predicteds)  # compute misclass error.

# 4.3 mice
library(mice)
# perform mice imputation, based on random forests.
miceMod <- mice(BostonHousing[, !names(BostonHousing) %in% "medv"], method="rf")  

miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)

# Lets compute the accuracy of ptratio.

actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds <- miceOutput[is.na(BostonHousing$ptratio), "ptratio"]
regr.eval(actuals, predicteds)

# Lets compute the accuracy of rad

actuals <- original$rad[is.na(BostonHousing$rad)]
predicteds <- miceOutput[is.na(BostonHousing$rad), "rad"]
mean(actuals != predicteds)  # compute misclass error.
