#' Apply a rectified linear unit (ReLU) to a vector/matrix.
#' 
#' @description Set the original input with negative values truncated to zero.
#' @param v A numeric vector or matrix
#' @return A numeric vector or matrix
#' @export
casl_util_ReLU <- function(v){
  v[v < 0] <- 0
  v
}




