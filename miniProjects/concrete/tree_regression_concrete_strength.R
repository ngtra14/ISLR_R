# Regression on the concrete strength
# ref : https://blogs.rstudio.com/tensorflow/posts/2019-12-10-variational-gaussian-process/
# not working due to packages inconsistencies

# switch to traditional tree regression and keras based regression
# The dataset -------------------------------------------------------------
library(readxl)
library(MASS)
library(tree)
library(randomForest)

library(keras)

concrete <- read_xls(
  "Concrete_Data.xls",
  col_names = c(
    "cement",
    "blast_furnace_slag",
    "fly_ash",
    "water",
    "superplasticizer",
    "coarse_aggregate",
    "fine_aggregate",
    "age",
    "strength"
  ),
  skip = 1
)

str(concrete)
head(concrete)

# EDA ---------------------------------------------------------------------

pairs(concrete)


# Linear, tree and random forest models -----------------------------------

# Simple linear regression model
set.seed(1)
train <- sample(1:nrow(concrete), nrow(concrete)*0.8)

lm_concrete <- lm(strength ~ ., data = concrete, subset = train)
summary(lm_concrete)

# tree regression model
tree_concrete <- tree(strength ~ ., data = concrete, subset = train)
summary(tree_concrete)

# random forest regression model
set.seed(508)
rf_concrete <- randomForest(strength ~ ., data = concrete, subset = train, 
                            mtry=4, importance=TRUE)
rf_concrete
varImpPlot(rf_concrete, type = 1)

# prediction on the test data
yhat_rf <- predict(rf_concrete, newdata = concrete[-train, ])
mean((yhat_rf - concrete[-train, ]$strength)^2) # 15.45983

yhat_tr <- predict(tree_concrete, newdata = concrete[-train, ])
mean((yhat_tr - concrete[-train, ]$strength)^2) # 68.22335

yhat_lm <- predict(lm_concrete, newdata = concrete[-train, ])
mean((yhat_lm - concrete[-train, ]$strength)^2) # 122.109



plot(yhat_lm, concrete[-train, ]$strength, col = "blue")
points(yhat_rf, concrete[-train, ]$strength, col = "red")
points(yhat_tr, concrete[-train, ]$strength, col = "green")
abline(0, 1)
legend("topleft", col = c("blue", "red", "green"), 
                  lty = c(1, 1, 1),
                  legend = c("lm", "rf", "tr"))



# use Keras in Regression model -------------------------------------------
# prepare the data format for nuerual net work
train_data <- concrete[train, 1:8]
train_target <- concrete[train, 9]
test_data <- concrete[-train, 1:8]
test_target <- concrete[-train, 9] 

# Preparing/scale the data
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
train_data <- scale(train_data, center = mean, scale = std)
test_data <- scale(test_data, center = mean, scale = std)

# convert to matrix ! important !
train_data <- as.matrix(train_data)
train_target <- as.matrix(train_target)
test_data <- as.matrix(test_data)
test_target <- as.matrix(test_target)

# display for inspections
mean
std
train_data[1, ]
test_data[1, ]

# construct neural network
# Because we will need to instantiate the same model multiple times,
# we use a function to construct it.


model <- keras_model_sequential() %>% 
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(train_data)[[2]]) %>% 
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dense(units = 1) 
  
model %>% compile(
    optimizer = "rmsprop", 
    loss = "mse", 
    metrics = c("mae")
  )


# Train the model (in silent mode, verbose=0)
model %>% fit(train_data, train_target,
              epochs = 100, batch_size = 8, verbose = 1)

# Evaluate the model on the validation data
results <- model %>% evaluate(test_data, test_target, verbose = 1)
results

yhat_tf <- model %>% predict(test_data)
mean((yhat_tf - concrete[-train, ]$strength)^2) # 130.5116/23.55472
mean((yhat_tf - test_target)^2) # 23.55472
