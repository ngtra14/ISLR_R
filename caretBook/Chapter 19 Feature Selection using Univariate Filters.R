# 19 Feature Selection using Univariate Filters ---------------------------
# 19.3 The Example
library(caret)
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
set.seed(10)
rfWithFilter <- sbf(x, y, sbfControl = filterCtrl)
rfWithFilter