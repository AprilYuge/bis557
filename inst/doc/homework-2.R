## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(bis557)
data("ridge_train")
data_scale <- as.data.frame(scale(ridge_train))
fit_ridge <- ridge_reg(y~. -1, data_scale, lambda = 1)
fit_ridge$coef

## ---- fig.width = 5, fig.height = 4, fig.align = 'center'----------------
# Scale the training and testing data sets respectively
data("ridge_train")
train_scale <- as.data.frame(scale(ridge_train))
data("ridge_test")
test_scale <- scale(ridge_test)

# Train models and calculate MSE on the testing data set for different lambdas
n <- 1000
lambda <- exp(seq(0, 5, length.out = n))
mse <- rep(0, n)
for (i in 1:n){
  fit_ridge <- ridge_reg(y ~. -1, lambda[i], train_scale)
  yhat <- test_scale[,-1] %*% fit_ridge$coef
  mse[i] <- mean((test_scale[, 1]-yhat)^2)
} 
plot(log(lambda), mse)

## ------------------------------------------------------------------------
lambda[which.min(mse)]

