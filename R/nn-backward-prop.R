#' Apply backward propagation algorithm.
#'
#' @description Backward propagation.
#' @param x A numeric vector representing one row of the input.
#' @param y A numeric vector representing one row of the response.
#' @param weights A list created by casl_nn_make_weights.
#' @param f_obj Output of the function casl_nn_forward_prop.
#' @param sigma_p Derivative of the activation function.
#' @param f_p Derivative of the loss function.
#' @return A list containing the derivative of the loss function
#' with respect to z (grad_z) and w (grad_w).
#' @export
casl_nn_backward_prop <- function(x, y, weights, f_obj, sigma_p, f_p){
  z <- f_obj$z; a <- f_obj$a
  L <- length(weights)
  grad_z <- vector("list", L)
  grad_w <- vector("list", L)
  for (j in rev(seq_len(L)))
  {
    if (j == L)
    {
      grad_z[[j]] <- f_p(y, a[[j]])
    } else {
      grad_z[[j]] <- (t(weights[[j + 1]]$w) %*%
                        grad_z[[j + 1]]) * sigma_p(z[[j]])
    }
    a_j1 <- if(j == 1) x else a[[j - 1L]]
    grad_w[[j]] <- grad_z[[j]] %*% t(a_j1)
  }
  list(grad_z=grad_z, grad_w=grad_w)
}