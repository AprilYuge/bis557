## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  fig.width = 5,
  fig.height = 4,
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
kernel_density <- function(x, h, x_new){
  den <- rep(0, times = length(x_new))
  for (i in seq_along(x_new)){
    xx <- (x - x_new[i])/h
    ind <- as.numeric(abs(xx) <= 1)
    den[i] <- mean(3/4*(1 - xx^2)*ind)/h
  }
  return(den)
}

## ------------------------------------------------------------------------
set.seed(2222)
# Normal distriburion
x <- rnorm(1000, 0, 1)
x_new <- seq(-3, 3, length.out = 100)
den <- kernel_density(x, h = 0.5, x_new)
plot(x_new, den, type = "l", ylim = c(0, 0.5), ylab = "density", xlab = "")
trueden <- dnorm(x_new, 0, 1)
points(x_new, trueden, type = "l", col = "red")
legend("topright", legend = c("kernel density","true density"), 
       lty = c(1, 1), col = c("black", "red"), cex = 0.8)
title("Normal Distribution N(0,1)")

# Beta distribution
x <- rbeta(1000, 2, 5)
x_new <- seq(0, 1, length.out = 100)
den <- kernel_density(x, h = 0.1, x_new)
plot(x_new, den, type = "l", ylim = c(0, 2.5), ylab = "density", xlab = "")
trueden <- dbeta(x_new, 2, 5)
points(x_new, trueden, type = "l", col = "red")
legend("topright", legend = c("kernel density","true density"), 
       lty = c(1, 1), col = c("black", "red"), cex = 0.8)
title("Beta Distribution Beta(2,5)")

# Gamma distribution
x <- rgamma(1000, 2, 2)
x_new <- seq(0, 5, length.out = 100)
den <- kernel_density(x, h = 0.3, x_new)
plot(x_new, den, type = "l", ylim = c(0, 1), ylab = "density", xlab = "")
trueden <- dgamma(x_new, 2, 2)
points(x_new, trueden, type = "l", col = "red")
legend("topright", legend = c("kernel density","true density"), 
       lty = c(1, 1), col = c("black", "red"), cex = 0.8)
title("Gamma Distribution Gamma(2,2)")

## ------------------------------------------------------------------------
set.seed(2222)
x <- rnorm(1000, 0, 1)
x_new <- seq(-3, 3, length.out = 100)

# h=0.1
den <- kernel_density(x, h = 0.1, x_new)
plot(x_new, den, type = "l", ylim = c(0, 0.5), ylab = "density", xlab = "")
trueden <- dnorm(x_new, 0, 1)
points(x_new, trueden, type = "l", col = "red")
legend("topright", legend = c("kernel density","true density"), 
       lty = c(1, 1), col = c("black", "red"), cex = 0.8)
title("N(0,1), h=0.1")

#h=0.5
den <- kernel_density(x, h = 0.5, x_new)
plot(x_new, den, type = "l", ylim = c(0, 0.5), ylab = "density", xlab = "")
trueden <- dnorm(x_new, 0, 1)
points(x_new, trueden, type = "l", col = "red")
legend("topright", legend = c("kernel density","true density"), 
       lty = c(1, 1), col = c("black", "red"), cex = 0.8)
title("N(0,1), h=0.5")

#h=2
den <- kernel_density(x, h = 2, x_new)
plot(x_new, den, type = "l", ylim = c(0, 0.5), ylab = "density", xlab = "")
trueden <- dnorm(x_new, 0, 1)
points(x_new, trueden, type = "l", col = "red")
legend("topright", legend = c("kernel density","true density"), 
       lty = c(1, 1), col = c("black", "red"), cex = 0.8)
title("N(0,1), h=2")

## ------------------------------------------------------------------------
lasso_reg_with_screening <- function(X, y, b, lambda, alpha){
  s <- rep(0, times = length(b))
  res <- y - X %*% b
  for (j in seq_along(b))
    s[j] <- (sum(X[, j]*res)/length(y) - lambda*(1 - alpha)*b[j])/(lambda*alpha)
  val <- (b == 0) & (abs(s) >= 1)
  return(val)
}

## ------------------------------------------------------------------------
set.seed(2222)
# Generate a random design matrix X
n <- 500L
p <- 1000L
X <- matrix(rnorm(n*p), nrow = n)
# Generate the real coefficient vector b with only the first ten numbers lager than zero
b <- c(seq(1, 0.1, length.out = 10L), rep(0, times = p - 10L))
# Generate the response vector y
y <- X %*% b +rnorm(n, mean = 0, sd = 0.1)
# Generate the current coefficient vector b_cur
b_cur <- rep(0, times = p)
# Show which current zero coefficients violate the KKT conditions
which(lasso_reg_with_screening(X, y, b_cur, lambda = 0.8, alpha = 0.8))

