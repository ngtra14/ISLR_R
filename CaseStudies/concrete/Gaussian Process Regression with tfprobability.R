# Gaussian Process Regression with tfprobability
# try to do it in a simple way in R base as much as possible

# The dataset -------------------------------------------------------------

library(tidyverse)
library(GGally)
library(visreg)
library(readxl)
library(rsample)
library(reticulate)
library(tfdatasets)
library(keras)
library(tfprobability)

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

concrete %>% glimpse()
head(concrete)

# EDA ---------------------------------------------------------------------
# pairs(concrete)
ggpairs(concrete)
pairs(concrete)

# water content
cement_ <- cut(concrete$cement, 3, labels = c("low", "medium", "high"))
fit <- lm(strength ~ (.) ^ 2, data = cbind(concrete[, 2:9], cement_))
summary(fit)

visreg(fit, "cement_", "water", gg = TRUE) + theme_minimal()

# data_cement_3 <- cbind(concrete[, 2:9], cement_)
# boxplot(as.factor(data_cement_3$cement_), data_cement_3$strength)


# Simple linear regression model ------------------------------------------

# scale predictors here already, so data are the same for all models
concrete[, 1:8] <- scale(concrete[, 1:8])

# train-test split 
set.seed(777)
split <- initial_split(concrete, prop = 0.8)
train <- training(split)
test <- testing(split)

# simple linear model with no interactions
fit1 <- lm(strength ~ ., data = train)
fit1 %>% summary()

# two-way interactions
fit2 <- lm(strength ~ (.) ^ 2, data = train)
fit2 %>% summary()

linreg_preds1 <- fit1 %>% predict(test[, 1:8])
linreg_preds2 <- fit2 %>% predict(test[, 1:8])

compare <-
  data.frame(
    y_true = test$strength,
    linreg_preds1 = linreg_preds1,
    linreg_preds2 = linreg_preds2
  )


# Data preparation for tensorflow format ----------------------------------

create_dataset <- function(df, batch_size, shuffle = TRUE) {
  
  df <- as.matrix(df)
  ds <-
    tensor_slices_dataset(list(df[, 1:8], df[, 9, drop = FALSE]))
  if (shuffle)
    ds <- ds %>% dataset_shuffle(buffer_size = nrow(df))
  ds %>%
    dataset_batch(batch_size = batch_size)
  
}

# just one possible choice for batch size ...
batch_size <- 64
train_ds <- create_dataset(train, batch_size = batch_size) # took very long
test_ds <- create_dataset(test, batch_size = nrow(test), shuffle = FALSE)


# The model ---------------------------------------------------------------
bt <- import("builtins")
RBFKernelFn <- reticulate::PyClass(
  "KernelFn",
  inherit = tensorflow::tf$keras$layers$Layer,
  list(
    `__init__` = function(self, ...) {
      kwargs <- list(...)
      super()$`__init__`(kwargs)
      dtype <- kwargs[["dtype"]]
      self$`_amplitude` = self$add_variable(initializer = initializer_zeros(),
                                            dtype = dtype,
                                            name = 'amplitude')
      self$`_length_scale` = self$add_variable(initializer = initializer_zeros(),
                                               dtype = dtype,
                                               name = 'length_scale')
      NULL
    },
    
    call = function(self, x, ...) {
      x
    },
    
    kernel = bt$property(
      reticulate::py_func(
        function(self)
          tfp$math$psd_kernels$ExponentiatedQuadratic(
            amplitude = tf$nn$softplus(array(0.1) * self$`_amplitude`),
            length_scale = tf$nn$softplus(array(2) * self$`_length_scale`)
          )
      )
    )
  )
)

# num_inducing_points <- 50
# 
# sample_dist <- tfd_uniform(low = 1, high = nrow(train) + 1)
# sample_ids <- sample_dist %>%
#   tfd_sample(num_inducing_points) %>%
#   tf$cast(tf$int32) %>%
#   as.numeric()
# sampled_points <- train[sample_ids, 1:8]

# terminate program due to lack consistency in packages
# Error: Python module tensorflow_probability was not found.

# model <- keras_model_sequential() %>%
#   layer_dense(units = 8,
#               input_shape = 8,
#               use_bias = FALSE) %>%
#   layer_variational_gaussian_process(
#     # number of inducing points
#     num_inducing_points = num_inducing_points,
#     # kernel to be used by the wrapped Gaussian Process distribution
#     kernel_provider = RBFKernelFn(),
#     # output shape 
#     event_shape = 1, 
#     # initial values for the inducing points
#     inducing_index_points_initializer = initializer_constant(as.matrix(sampled_points)),
#     unconstrained_observation_noise_variance_initializer =
#       initializer_constant(array(0.1))
#   )


