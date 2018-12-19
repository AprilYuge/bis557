#' Create list of weights to describe a dense neural network.
#'
#' @description A function to return a list of weights in a 
#' dense neural network.
#' @param sizes A vector giving the size of each layer, including 
#' the input and output layers.
#' @return A list containing initialized weights and biases.
#' @importFrom stats rnorm
#' @export
casl_nn_make_weights <- function(sizes){
  L <- length(sizes) - 1L
  weights <- vector("list", L)
  for (j in seq_len(L))
  {
    w <- matrix(rnorm(sizes[j] * sizes[j + 1L]),
                ncol = sizes[j],
                nrow = sizes[j + 1L])
    weights[[j]] <- list(w=w,
                         b=rnorm(sizes[j + 1L]))
  }
  weights
}