# Sonar dataset - binary classification -----------------------------------
library(caret)
library(mlbench)
data(Sonar)

head(Sonar)
table(Sonar$Class)
Sonar$Class <- as.factor(Sonar$Class)


# Split dataset -----------------------------------------------------------

set.seed(107)

inTrain <- createDataPartition(y=Sonar$Class,
                               p = 0.75,
                               list = FALSE)

training <- Sonar[inTrain, ]
testing <- Sonar[-inTrain, ]
nrow(training)
nrow(testing)


# Logistic regression -----------------------------------------------------

logReg <- glm(Class ~. , data = training, family = "binomial")


# Prediction and performance ----------------------------------------------

prob <- predict(logReg, testing, type = "response")
pred <- ifelse(prob > 0.5, "M", "R")
table(testing$Class, pred)
confusionMatrix(testing$Class, as.factor(pred))


# Using deep learning neural networks -------------------------------------
library(keras)

y <- training[, "Class"]
x <- training[, 1:60]

y_test <- testing[, "Class"]
x_test <- testing[, 1:60]

# scale data --------------------------------------------------------------

# scale to [0,1]
x <- as.matrix(apply(x, 2, function(x) (x-min(x))/(max(x) - min(x))))
x_test <- as.matrix(apply(x_test, 2, function(x) (x-min(x))/(max(x) - min(x))))

# # one hot encode classes / create DummyFeatures -------------------------
levels(y) <- 1:2
y <- to_categorical(as.integer(y) - 1 , num_classes = 2)

levels(y_test) <- 1:2
y_test <- to_categorical(as.integer(y_test) - 1 , num_classes = 2)

# construct model ---------------------------------------------------------
# create sequential model
model <- keras_model_sequential()

# add layers, first layer needs input dimension
model %>%
  layer_dense(input_shape = ncol(x), units = 60, activation = "relu") %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 2, activation = "softmax")

# add a loss function and optimizer
model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

# fit model with training data set, 200 times
fit <- model %>%
  fit(
    x = x,
    y = y,
    shuffle = T,
    batch_size = 5,
    validation_split = 0.8,
    epochs = 200
  )


# plot accuracy -----------------------------------------------------------

plot(fit)

model %>% evaluate(x_test, y_test)
# $acc
# [1] 0.4705882

# DNN with dropout --------------------------------------------------------
model <-  keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(x), units = 60, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 2, activation = "softmax")

model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

fit = model %>%
  fit(
    x = x,
    y = y,
    shuffle = T,
    validation_split = 0.2,
    epochs = 200,
    batch_size = 5
  )
plot(fit)

model %>% evaluate(x_test, y_test)
# $acc
# [1] 0.6666667
