
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats lm
#' @importFrom stats model.matrix
#' @importFrom stats model.frame
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  # To get the design matrix X
  x <- model.matrix(formula,data)
  
  # To get the dependent variable y
  model <- model.frame(formula,data) 
  y <- model[,1]
  
  #QR decomposition of design matrix
  qrr <- qr(x)
  qrr$tol <- 1e-07
  
  #Terms of the model
  tm <- attr(model,"terms")
  
  #Effects of the model
  eff <- qr.qty(qrr,y)
  nameeff <- colnames(x)
  nameeff <- nameeff[qrr$pivot[1:qrr$rank]]
  names(eff) <- c(nameeff, rep("", length(eff)-length(nameeff)))
  
  #Form the list
  li <- list()
    li$coefficients <- qr.coef(qrr,y)
  li$residuals <- qr.resid(qrr,y)
  li$effects <- eff
  li$rank <- qrr$rank
  li$fitted.values <- qr.fitted(qrr,y)
  li$assign <- attr(x,"assign")
  li$qr <- qrr
  li$df.residual <- nrow(x)-qrr$rank
  li$contrasts <- attr(x,"contrasts")
  li$xlevels <- .getXlevels(tm,model)
  li$call <- match.call()
  li$terms <- tm
  li$model <- model
  class(li) <- "lm"
  li
}
