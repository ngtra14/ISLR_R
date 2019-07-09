######################################################################
### Create two character vectors for different predictor sets. One
### will have all the predictors (called 'fullSet').
##
### Another has some of the sparse predictors removed for models that
### require such filtering. This will be called 'reducedSet'
### (predictors without sparse or Near Zero Variance predictors). This
### set will also have predictors removed that are almost completely
### correlated with other predictors

fullSet <- names(training)[names(training) != "Class"]

isNZV <- nearZeroVar(training[,fullSet], saveMetrics = TRUE, freqCut = floor(nrow(training)/5))
fullSet <-  rownames(subset(isNZV, !nzv))
str(fullSet)

reducedSet <- rownames(subset(isNZV, !nzv & freqRatio < floor(nrow(training)/50)))

### Perfectly collinear predictors (due to their construction) March
### and Sunday were selected because they have the lowest frequency of
### all months and days
reducedSet <- reducedSet[(reducedSet != "allPub") &
                         (reducedSet != "numPeople") &
                         (reducedSet != "Mar") &
                         (reducedSet != "Sun")
                         ]

### all months and days
reducedSet <- reducedSet[(reducedSet != "allPub") &
                         (reducedSet != "numPeople") &
                         (reducedSet != "Mar") &
                         (reducedSet != "Sun")
                         ]
str(reducedSet)
