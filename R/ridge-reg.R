#' Fit a ridge regression model
#'
#' @description This function passes parameters to the ridge_reg function.
#' @param formula a formula
#' @param data a data.frame
#' @param lambda a lambda value for penalty
#' @return A ridge_reg object
#' @importFrom stats model.matrix
#' @importFrom stats model.frame
#' @examples
#' iris_scale <- as.data.frame(scale(iris))
#' ridge_fit <- ridge_reg(Sepal.Length ~. -1, iris_scale, lambda = 1)
#' ridge_fit$coef
#' @export
ridge_reg <- function(formula, lambda, data) {
  
  # Get the scaled design matrix X
  x <- model.matrix(formula, data)
  # if (colnames(x)[1] == "(Intercept)")
  #   x <- x[,-1]
  # n <- nrow(x)
  # p <- ncol(x)
  # x <- x - matrix(rep(1,n), nrow = n) %*% colMeans(x)
  # xscale <- (rep(1/n, n) %*% x^2)^0.5
  # x <- x/rep(xscale, rep(n, p))
  
  # Get the scaled dependent variable y
  y <- model.frame(formula, data)[, 1]
  # y <- y-mean(y)
  
  # Singular value decomposition of X
  svd_x <- svd(x)
  u <- svd_x$u
  v <- svd_x$v
  d <- svd_x$d
  
  # Calculate coefficients
  co <- v %*% diag(d/(d^2+lambda)) %*% t(u) %*% y
  rownames(co) <- colnames(x)
  
  # Form the return list
  li <- list()
  li$coef <- co
  li$svd <- svd_x
  
  # Return the object
  class(li) <- "ridge_reg"
  li
}
