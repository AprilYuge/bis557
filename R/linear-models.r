
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats lm
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  x <- model.matrix(formula,data)
  # To get the dependent variable
  cnames <- colnames(data)
  depvar <- all.vars(formula)[1]
  tag <- cnames == depvar
  y <- data[,tag]
  qrr <- qr(x)
  co <- qr.coef(qrr,y)
  re <- qr.resid(qrr,y)
  call <- paste0("linear_model(formula = ", deparse(formula), ", data = lm_patho")
  li <- list()
  li$call <- call
  li$coefficients <- co
  li$residuals <- re
  li$qr <- qrr
  class(li) <- "lm"
  li
}
