#' Apply forward propagation algorithm.
#'
#' @description Forward propagation.
#' @param x A numeric vector representing one row of the input
#' @param weights A list created by casl_nn_make_weights
#' @param sigma The activation function
#' @return A list containing the new weighted responses (z) and activations (a)
#' @export
casl_nn_forward_prop <- function(x, weights, sigma){
  L <- length(weights)
  z <- vector("list", L)
  a <- vector("list", L)
  for (j in seq_len(L))
  {
    a_j1 <- if(j == 1) x else a[[j - 1L]]
    z[[j]] <- weights[[j]]$w %*% a_j1 + weights[[j]]$b
    a[[j]] <- if (j != L) sigma(z[[j]]) else z[[j]]
  }
  list(z=z, a=a)
}