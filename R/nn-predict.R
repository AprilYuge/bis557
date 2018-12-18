#' Predict values from a training neural network.
#'
#' @description predict outcomes by neural network.
#' @param weights List of weights describing the neural network.
#' @param X_test A numeric data matrix for the predictions.
#' @return A matrix of predicted values.
#' @export
casl_nn_predict <- function(weights, X_test){
  p <- length(weights[[length(weights)]]$b)
  y_hat <- matrix(0, ncol = p, nrow = nrow(X_test))
  for (i in seq_len(nrow(X_test)))
  {
    a <- casl_nn_forward_prop(X_test[i,], weights,
                              casl_util_ReLU)$a
    y_hat[i, ] <- a[[length(a)]]
  }
  y_hat 
}