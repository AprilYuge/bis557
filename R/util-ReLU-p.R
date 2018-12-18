#' Apply derivative of the rectified linear unit (ReLU).
#'
#' @description Set positive values to 1 and negative values to zero
#' @param v A numeric vector or matrix
#' @return A numeric vector or matrix
#' @export
casl_util_ReLU_p <- function(v){
  p <- v * 0
  p[v > 0] <- 1
  p 
}