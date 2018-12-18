#' Derivative of the mean absolute error (MAE) function.
#'
#' @description Current derivative of the MAE function
#' @param y A numeric vector of responses.
#' @param a A numeric vector of predicted responses.
#' @return A numeric vector of the derivative.
#' @export
casl_util_mae_p <- function(y, a){
  if (a > y)
    1
  else
    -1
}