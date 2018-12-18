#' Apply stochastic gradient descent (SGD) to estimate the weights of 
#' neural network with MAE as the loss function.
#'
#' @description Estimate weights in a neural network.
#' @param X A numeric data matrix.
#' @param y A numeric vector of responses.
#' @param sizes A numeric vector giving the sizes of layers in
#' the neural network.
#' @param epochs Integer number of epochs to computer.
#' @param eta Positive numeric learning rate.
#' @param weights Optional list of starting weights.
casl_nn_sgd_mae <- function(X, y, sizes, epochs, eta, weights=NULL){
  if (is.null(weights))
  {
    weights <- casl_nn_make_weights(sizes)
  }
  for (epoch in seq_len(epochs))
  {
    for (i in seq_len(nrow(X)))
    {
      f_obj <- casl_nn_forward_prop(X[i,], weights,
                                    casl_util_ReLU)
      b_obj <- casl_nn_backward_prop(X[i,], y[i,], weights,
                                     f_obj, casl_util_ReLU_p,
                                     casl_util_mae_p)
      for (j in seq_along(b_obj))
      {
        weights[[j]]$b <- weights[[j]]$b -
          eta * b_obj$grad_z[[j]]
        weights[[j]]$w <- weights[[j]]$w -
          eta * b_obj$grad_w[[j]]
      }
    }
  }
  weights
}