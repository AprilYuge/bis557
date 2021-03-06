% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ridge-reg.R
\name{ridge_reg}
\alias{ridge_reg}
\title{Fit a ridge regression model}
\usage{
ridge_reg(formula, lambda, data)
}
\arguments{
\item{formula}{a formula}

\item{lambda}{a lambda value for penalty}

\item{data}{a data.frame}
}
\value{
A ridge_reg object
}
\description{
This function passes parameters to the ridge_reg function.
}
\examples{
data("ridge_train")
data_scale <- as.data.frame(scale(ridge_train))
ridge_fit <- ridge_reg(y ~. -1, data_scale, lambda = 1)
ridge_fit$coef
}
