# 17 Measuring Performance ------------------------------------------------
# 17.1 Measures for Regression
library(mlbench)
data(BostonHousing)
library(caret)

set.seed(280)
bh_index <- createDataPartition(BostonHousing$medv, p = .75, list = FALSE)
bh_tr <- BostonHousing[ bh_index, ]
bh_te <- BostonHousing[-bh_index, ]

set.seed(7279)
lm_fit <- train(medv ~ . + rm:lstat,
                data = bh_tr, 
                method = "lm")
bh_pred <- predict(lm_fit, bh_te)

lm_fit

postResample(pred = bh_pred, obs = bh_te$medv)

# 17.2 Measures for Predicted Classes
# produce some test data
set.seed(144)
true_class <- factor(sample(paste0("Class", 1:2), 
                            size = 1000,
                            prob = c(.2, .8), replace = TRUE))
true_class <- sort(true_class)
class1_probs <- rbeta(sum(true_class == "Class1"), 4, 1)
class2_probs <- rbeta(sum(true_class == "Class2"), 1, 2.5)
test_set <- data.frame(obs = true_class,
                       Class1 = c(class1_probs, class2_probs))
test_set$Class2 <- 1 - test_set$Class1
test_set$pred <- factor(ifelse(test_set$Class1 >= .5, "Class1", "Class2"))

# plot data
ggplot(test_set, aes(x = Class1)) + 
  geom_histogram(binwidth = .05) + 
  facet_wrap(~obs) + 
  xlab("Probability of Class #1")

confusionMatrix(data = test_set$pred, reference = test_set$obs)
confusionMatrix(data = test_set$pred, reference = test_set$obs, mode = "prec_recall")

postResample(pred = test_set$pred, obs = test_set$obs)

# 17.3 Measures for Class Probabilities
twoClassSummary(test_set, lev = levels(test_set$obs))
prSummary(test_set, lev = levels(test_set$obs))
mnLogLoss(test_set, lev = levels(test_set$obs))

# 17.4 Lift Curves
set.seed(2)
lift_training <- twoClassSim(1000)
lift_testing  <- twoClassSim(1000)

ctrl <- trainControl(method = "cv", classProbs = TRUE,
                     summaryFunction = twoClassSummary)

set.seed(1045)
fda_lift <- train(Class ~ ., data = lift_training,
                  method = "fda", metric = "ROC",
                  tuneLength = 20,
                  trControl = ctrl)
set.seed(1045)
lda_lift <- train(Class ~ ., data = lift_training,
                  method = "lda", metric = "ROC",
                  trControl = ctrl)

library(C50)
set.seed(1045)
c5_lift <- train(Class ~ ., data = lift_training,
                 method = "C5.0", metric = "ROC",
                 tuneLength = 10,
                 trControl = ctrl,
                 control = C5.0Control(earlyStopping = FALSE))

## Generate the test set results
lift_results <- data.frame(Class = lift_testing$Class)
lift_results$FDA <- predict(fda_lift, lift_testing, type = "prob")[,"Class1"]
lift_results$LDA <- predict(lda_lift, lift_testing, type = "prob")[,"Class1"]
lift_results$C5.0 <- predict(c5_lift, lift_testing, type = "prob")[,"Class1"]
head(lift_results)

trellis.par.set(caretTheme())
lift_obj <- lift(Class ~ FDA + LDA + C5.0, data = lift_results)
plot(lift_obj, values = 60, auto.key = list(columns = 3,
                                            lines = TRUE,
                                            points = FALSE))
ggplot(lift_obj, values = 60)

# 17.5 Calibration Curves

trellis.par.set(caretTheme())
cal_obj <- calibration(Class ~ FDA + LDA + C5.0,
                       data = lift_results,
                       cuts = 13)
plot(cal_obj, type = "l", auto.key = list(columns = 3,
                                          lines = TRUE,
                                          points = FALSE))
ggplot(cal_obj)
