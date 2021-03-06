## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  fig.width = 6,
  fig.height = 4.5,
  fig.align = 'center',
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  library(glmnet)
#  library(keras)
#  
#  # Get 1000 samples from the training set for training.
#  mnist <- dataset_mnist()
#  n_train <- nrow(mnist$train$x)
#  s <- sample(seq_len(n_train), 1000)
#  
#  # Get the training set
#  x_train <- array_reshape(mnist$train$x[s,,], c(60000, 28^2))/255
#  y_train <- factor(mnist$train$y[s])
#  
#  # Get the testing set
#  x_test <- array_reshape(mnist$test$x, c(10000, 28^2))/255
#  y_test <- factor(mnist$test$y)
#  
#  # Fit the LASSO model
#  fit <- cv.glmnet(x_train, y_train, family = "multinomial")
#  
#  # Get prediction accuracy of the training set
#  pred_train <- predict(fit$glmnet.fit, x_train, s = fit$lambda.min, type = "class")
#  t_train <- table(as.vector(pred_train), y_train)
#  sum(diag(t_train))/sum(t_train)
#  
#  # Get prediction accuracy of the testing set
#  pred_test <- predict(fit$glmnet.fit, x_test, s = fit$lambda.min, type = "class")
#  t_test <- table(as.vector(pred_test), y_test)
#  sum(diag(t_test))/sum(t_test)

## ---- eval=FALSE---------------------------------------------------------
#  # Some parameters
#  batch_size <- 2
#  num_classes <- 10
#  epochs <- 10
#  
#  # Get the training set
#  x_train <- array_reshape(mnist$train$x[s,,], c(length(s), 28, 28, 1))/255
#  y_train <- to_categorical(mnist$train$y[s], num_classes)
#  
#  # Get the testing set
#  x_test <- array_reshape(mnist$test$x, c(nrow(mnist$test$x), 28, 28, 1))/255
#  y_test <- to_categorical(mnist$test$y, num_classes)
#  
#  # Define model
#  model <- keras_model_sequential()
#  
#  model %>%
#    layer_conv_2d(filters = 32, kernel_size = c(3,3),
#                  input_shape = c(28,28,1),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_conv_2d(filters = 64, kernel_size = c(3,3),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_flatten() %>%
#    layer_dense(units = 128) %>%
#    layer_activation(activation = "relu") %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_dense(units = num_classes) %>%
#    layer_activation(activation = "softmax")
#  
#  # Compile model
#  model %>% compile(
#    loss = loss_categorical_crossentropy,
#    optimizer = optimizer_adadelta(),
#    metrics = c('accuracy')
#  )
#  
#  # Train model
#  model %>% fit(
#    x_train, y_train,
#    batch_size = batch_size,
#    epochs = epochs,
#    validation_split = 0.1
#  )
#  
#  # Get prediction accuracy of the training set
#  pred_train <- predict_classes(model, x_train)
#  sum(pred_train == mnist$train$y[s])/length(s)
#  
#  # Get prediction accuracy of the testing set
#  pred_test <- predict_classes(model, x_test)
#  sum(pred_test == mnist$test$y)/length(mnist$test$y)

## ---- eval=FALSE---------------------------------------------------------
#  library(keras)
#  
#  # Load the EMNIST data set
#  load("x_28.Rdata")
#  load("emnist.Rdata")
#  
#  # Get the training and testing sets
#  X <- array_reshape(x_28, c(nrow(emnist), 28, 28, 1))/255
#  Y <- to_categorical(emnist$class, num_classes=26L)
#  X_train <- X[emnist$train_id == "train",,,,drop=FALSE]
#  X_valid <- X[emnist$train_id != "train",,,,drop=FALSE]
#  Y_train <- Y[emnist$train_id == "train",]
#  Y_valid <- Y[emnist$train_id != "train",]
#  
#  # Model 1
#  model1 <- keras_model_sequential()
#  
#  model1 %>%
#    layer_conv_2d(filters = 32, kernel_size = c(2,2),
#                  input_shape = c(28,28,1),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_conv_2d(filters = 32, kernel_size = c(2,2),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_conv_2d(filters = 32, kernel_size = c(2,2),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_conv_2d(filters = 32, kernel_size = c(2,2),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_flatten() %>%
#    layer_dense(units = 128) %>%
#    layer_activation(activation = "relu") %>%
#    layer_dense(units = 128) %>%
#    layer_activation(activation = "relu") %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_dense(units = 26) %>%
#    layer_activation(activation = "softmax")
#  
#  model1 %>% compile(loss = loss_categorical_crossentropy,
#                    optimizer = optimizer_rmsprop(),
#                    metrics = c('accuracy'))
#  
#  history1 <- model1 %>%
#    fit(X_train, Y_train, batch_size = 128, epochs = 2,
#        validation_data = list(X_valid, Y_valid))
#  
#  # Get the prediction accuracy for training and testing sets
#  emnist$predict1 <- predict_classes(model1, X)
#  tapply(emnist$predict1 == emnist$class, emnist$train_id, mean)
#  
#  # Model 2
#  model2 <- keras_model_sequential()
#  
#  model2 %>%
#    layer_conv_2d(filters = 32, kernel_size = c(3,3),
#                  input_shape = c(28,28,1),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_conv_2d(filters = 32, kernel_size = c(3,3),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_conv_2d(filters = 32, kernel_size = c(3,3),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_conv_2d(filters = 32, kernel_size = c(3,3),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_flatten() %>%
#    layer_dense(units = 128) %>%
#    layer_activation(activation = "relu") %>%
#    layer_dense(units = 128) %>%
#    layer_activation(activation = "relu") %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_dense(units = 26) %>%
#    layer_activation(activation = "softmax")
#  
#  model2 %>% compile(loss = loss_categorical_crossentropy,
#                    optimizer = optimizer_rmsprop(),
#                    metrics = c('accuracy'))
#  
#  history2 <- model2 %>%
#    fit(X_train, Y_train, batch_size = 128, epochs = 2,
#        validation_data = list(X_valid, Y_valid))
#  
#  # Get the prediction accuracy for training and testing sets
#  emnist$predict2 <- predict_classes(model2, X)
#  tapply(emnist$predict2 == emnist$class, emnist$train_id, mean)
#  
#  # Model 3
#  model3 <- keras_model_sequential()
#  
#  model3 %>%
#    layer_conv_2d(filters = 64, kernel_size = c(3,3),
#                  input_shape = c(28,28,1),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_conv_2d(filters = 64, kernel_size = c(3,3),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_conv_2d(filters = 64, kernel_size = c(3,3),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_conv_2d(filters = 64, kernel_size = c(3,3),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_flatten() %>%
#    layer_dense(units = 128) %>%
#    layer_activation(activation = "relu") %>%
#    layer_dense(units = 128) %>%
#    layer_activation(activation = "relu") %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_dense(units = 26) %>%
#    layer_activation(activation = "softmax")
#  
#  model3 %>% compile(loss = loss_categorical_crossentropy,
#                    optimizer = optimizer_rmsprop(),
#                    metrics = c('accuracy'))
#  
#  history3 <- model3 %>%
#    fit(X_train, Y_train, batch_size = 128, epochs = 2,
#        validation_data = list(X_valid, Y_valid))
#  
#  # Get the prediction accuracy for training and testing sets
#  emnist$predict3 <- predict_classes(model3, X)
#  tapply(emnist$predict3 == emnist$class, emnist$train_id, mean)
#  
#  # Model 4
#  model4 <- keras_model_sequential()
#  
#  model4 %>%
#    layer_conv_2d(filters = 64, kernel_size = c(3,3),
#                  input_shape = c(28,28,1),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_conv_2d(filters = 64, kernel_size = c(3,3),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_conv_2d(filters = 64, kernel_size = c(3,3),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_conv_2d(filters = 64, kernel_size = c(3,3),
#                  padding = "same") %>%
#    layer_activation(activation = "relu") %>%
#    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_flatten() %>%
#    layer_dense(units = 256) %>%
#    layer_activation(activation = "relu") %>%
#    layer_dense(units = 256) %>%
#    layer_activation(activation = "relu") %>%
#    layer_dropout(rate = 0.5) %>%
#    layer_dense(units = 26) %>%
#    layer_activation(activation = "softmax")
#  
#  model4 %>% compile(loss = loss_categorical_crossentropy,
#                     optimizer = optimizer_rmsprop(),
#                     metrics = c('accuracy'))
#  
#  history4 <- model4 %>%
#    fit(X_train, Y_train, batch_size = 128, epochs = 2,
#        validation_data = list(X_valid, Y_valid))
#  
#  # Get the prediction accuracy for training and testing sets
#  emnist$predict4 <- predict_classes(model3, X)
#  tapply(emnist$predict4 == emnist$class, emnist$train_id, mean)

## ---- echo=FALSE---------------------------------------------------------
library(knitr)
acc <- matrix(c(0.8870, 0.8837, 0.9009, 0.8999, 0.9205, 
                0.9175, 0.9318, 0.9273), nrow = 2)
rownames(acc) <- c("train", "test")
colnames(acc) <- c("Model 1", "Model 2", "Model 3", "Model 4")
kable(acc, caption = "Table1: Prediction Accuracy for each Model")

## ---- eval=FALSE---------------------------------------------------------
#  casl_util_mae_p <- function(y, a){
#    if (a > y)
#      1
#    else
#      -1
#  }

## ---- eval=FALSE---------------------------------------------------------
#  casl_nn_sgd_mae <- function(X, y, sizes, epochs, eta, weights=NULL){
#    if (is.null(weights))
#    {
#      weights <- casl_nn_make_weights(sizes)
#    }
#    for (epoch in seq_len(epochs))
#    {
#      for (i in seq_len(nrow(X)))
#      {
#        f_obj <- casl_nn_forward_prop(X[i,], weights,
#                                      casl_util_ReLU)
#        b_obj <- casl_nn_backward_prop(X[i,], y[i,], weights,
#                                       f_obj, casl_util_ReLU_p,
#                                       casl_util_mae_p)
#        for (j in seq_along(b_obj))
#        {
#          weights[[j]]$b <- weights[[j]]$b -
#            eta * b_obj$grad_z[[j]]
#          weights[[j]]$w <- weights[[j]]$w -
#            eta * b_obj$grad_w[[j]]
#        }
#      }
#    }
#    weights
#  }

## ---- eval=FALSE---------------------------------------------------------
#  library(bis557)
#  
#  set.seed(222)
#  
#  # Simulate some data for training
#  n <- 50
#  X_train <- matrix(seq(-1, 1, length.out = n))
#  y_train <- 2*X_train + 3 + rnorm(n, sd = 0.3)
#  
#  # Pick some data as outliers
#  y_train[3] <- y_train[3] - 7
#  y_train[10] <- y_train[10] - 5
#  y_train[22] <- y_train[22] - 4
#  y_train[35] <- y_train[35] + 3.5
#  y_train[44] <- y_train[44] + 6
#  
#  # Simulate some data for testing
#  X_test <- matrix(runif(n, min=-1, max=1), ncol=1)
#  y_test <- 2*X_test + 3 + rnorm(n, sd = 0.3)
#  
#  # SGD with MSE as the loss function
#  weights_mse <- casl_nn_sgd_mse(X_train, y_train, sizes=c(1, 25, 1),epochs=25, eta=0.01)
#  # Prediction on the training set
#  y_pred_train_mse <- casl_nn_predict(weights_mse, X_train)
#  # Prediction on the testing set
#  y_pred_test_mse <- casl_nn_predict(weights_mse, X_test)
#  
#  # SGD with MAE as the loss function
#  weights_mae <- casl_nn_sgd_mae(X_train, y_train, sizes=c(1, 25, 1),epochs=25, eta=0.01)
#  # Prediction on the training set
#  y_pred_train_mae <- casl_nn_predict(weights_mae, X_train)
#  # Prediction on the testing set
#  y_pred_test_mae <- casl_nn_predict(weights_mae, X_test)
#  
#  # Compare the performance
#  # MSE of the training set with MSE as the loss function
#  mse_train_mse <- mean((y_pred_train_mse - y_train)^2)
#  # MAE of the training set with MSE as the loss function
#  mae_train_mse <- mean(abs(y_pred_train_mse - y_train))
#  # MSE of the testing set with MSE as the loss function
#  mse_test_mse <- mean((y_pred_test_mse - y_test)^2)
#  # MAE of the testing set with MSE as the loss function
#  mae_test_mse <- mean(abs(y_pred_test_mse - y_test))
#  
#  # MSE of the training set with MAE as the loss function
#  mse_train_mae <- mean((y_pred_train_mae - y_train)^2)
#  # MAE of the training set with MAE as the loss function
#  mae_train_mae <- mean(abs(y_pred_train_mae - y_train))
#  # MSE of the testing set with MAE as the loss function
#  mse_test_mae <- mean((y_pred_test_mae - y_test)^2)
#  # MAE of the testing set with MAE as the loss function
#  mae_test_mae <- mean(abs(y_pred_test_mae - y_test))

## ---- echo=FALSE---------------------------------------------------------
library(knitr)
result <- matrix(c(2.5118, 0.8856, 0.2806, 0.4494, 2.7656, 0.7214, 0.1041, 0.2392), ncol = 2)
colnames(result) <- c("Loss Function: MSE", "Loss Function: MAE")
rownames(result) <- c("Train MSE", "Train MAE", "Test MSE", "Test MAE")
kable(result, caption = "Table 2: Results Somparison for Two Loss Functions")

## ---- eval=FALSE, echo=FALSE---------------------------------------------
#  plot(X_train, y_train)
#  lines(X_train, y_pred_train_mse, col=1)
#  lines(X_train, y_pred_train_mae, col=2)
#  title("Prediction on the Training Set")
#  legend("topleft", lty = c(1,1), col = c(1,2),
#         legend = c("MSE", "MAE"), title = "Loss Function")
#  
#  plot(X_test,y_test)
#  lines(X_test[order(X_test)], y_pred_test_mse[order(X_test)], col=1)
#  lines(X_test[order(X_test)], y_pred_test_mae[order(X_test)], col=2)
#  title("Prediction on the Testing Set")
#  legend("topleft", lty = c(1,1), col = c(1,2),
#         legend = c("MSE", "MAE"), title = "Loss Function")

