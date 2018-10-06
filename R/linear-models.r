
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats model.matrix
#' @importFrom stats model.frame
#' @importFrom stats .getXlevels 
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  # Get the design matrix X
  x <- model.matrix(formula, data)
  
  # Get the dependent variable y
  model <- model.frame(formula, data) 
  y <- model[, 1]
  
  # QR decomposition of design matrix
  qrr <- qr(x)
  qrr$tol <- 1e-07
  
  # Generate coefficients
  co <- qr.coef(qrr, y)
  
  # Generate residuals
  re <- qr.resid(qrr, y)
  names(re) <- 1:length(re)
  
  # Generate effects
  eff <- qr.qty(qrr, y)
  nameeff <- colnames(x)
  nameeff <- nameeff[qrr$pivot[1:qrr$rank]]
  names(eff) <- c(nameeff, rep("", length(eff)-length(nameeff)))
  
  # Generate fitted values
  fit <- qr.fitted(qrr, y)
  names(fit) <- 1: length(fit)
  
  #Terms of the model
  tm <- attr(model, "terms")
  
  #Form the list
  li <- list()
  li$coefficients <- co
  li$residuals <- re
  li$effects <- eff
  li$rank <- qrr$rank
  li$fitted.values <- fit
  li$assign <- attr(x, "assign")
  li$qr <- qrr
  li$df.residual <- nrow(x)-qrr$rank
  li$contrasts <- attr(x, "contrasts")
  li$xlevels <- .getXlevels(tm, model)
  li$call <- match.call()
  li$terms <- tm
  li$model <- model
  li$na.action <- attr(model, "na.action")
  class(li) <- "lm"
  li
}