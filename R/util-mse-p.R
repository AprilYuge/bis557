#' Derivative of the mean squared error (MSE) function.
#'
#' @description Current derivative of the MSE function
#' @param y A numeric vector of responses.
#' @param a A numeric vector of predicted responses.
#' @return A numeric vector of the derivative.
#' @export
casl_util_mse_p <- function(y, a){
  (a - y)
}
