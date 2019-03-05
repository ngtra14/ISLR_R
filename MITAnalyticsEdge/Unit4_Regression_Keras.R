# Boston housing price prediction
# reference : https://keras.rstudio.com/articles/tutorial_basic_regression.html
library(keras)

# Read in data
boston <- read.csv("boston.csv")
str(boston)


# Split the data
library(caTools)
set.seed(123)
split <- sample.split(boston$MEDV, SplitRatio = 0.7)
train <- subset(boston, split==TRUE)
test <- subset(boston, split==FALSE)

train$TOWN <- NULL
test$TOWN <- NULL

train_data <- train[, -4]
train_labels <- train[, 4]

test_data <- test[, -4]
test_labels <- test[, 4]

# Normalize training data
train_data <- scale(train_data) 

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_data, "scaled:center") 
col_stddevs_train <- attr(train_data, "scaled:scale")
test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)

# Create the model
build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()

# Train the model
# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 500
# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

library(ggplot2)
plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(ylim = c(0, 5))

# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)
model <- build_model()
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)
plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 150), ylim = c(0, 5))

c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))
paste0("Mean absolute error on test set: $", sprintf("%.2f", mae * 1000))

test_predictions <- model %>% predict(test_data)
test_predictions[ , 1]

keras_model_sse <- sum((test_predictions[,1] - test_labels)^2)
keras_model_sse
# 2712.715