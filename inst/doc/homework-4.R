## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
# Generate matrices X, XX, vector p, and matrix XWX.
X <- matrix(rnorm(4), nrow = 2)
XX <- crossprod(X)
p <- c(0.5, 1e-15)
XWX <- crossprod(X, diag(p*(1-p)) %*% X)
# Calculate the condition number of matrices XX and XWX.
kappa(XX)
kappa(XWX)

## ------------------------------------------------------------------------
irwls_glm_ridge <- function(X, y, family, maxit = 100, tol = 1e-10, lambda){
  p <- ncol(X)
  beta <- rep(0, p)
  for(j in seq_len(maxit)){
    b_old <- beta
    eta <- X %*% beta
    mu <- family$linkinv(eta)
    mu_p <- family$mu.eta(eta)
    z <- eta + (y - mu) / mu_p
    W <- as.numeric(mu_p^2 / family$variance(mu))
    XtX <- crossprod(X, diag(W) %*% X)
    Xtz <- crossprod(X, W * z)
    beta <- solve(XtX + lambda*diag(p), Xtz)
    if(mean(crossprod(beta - b_old)) < tol) 
      break
  }
  return(beta)
}

## ------------------------------------------------------------------------
set.seed(2222)
n <- 10000
p <- 10
beta <- matrix(c(1, 0.5, 0.1, rep(0, p-3)), nrow = p)
X <- cbind(1, matrix(rnorm(n*(p-1)), nrow = n))
prob <- 1/(1 + exp(- X %*% beta))
y <- as.numeric(runif(n) < prob)
beta_hat <- irwls_glm_ridge(X, y, family = binomial(link = "logit"), lambda = 1)
# Compare the estimated beta to the true beta
cbind(beta, beta_hat)

## ------------------------------------------------------------------------
# Set a new class "sparse.matrix".
setClass("sparse.matrix", representation(i = "integer", j = "integer", dims = "integer", x = "numeric"))

# Function sparse.matrix can return a "sparse.matrix" object.
sparse.matrix <- function(i, j, x, dims){
  i <- as.integer(i)
  j <- as.integer(j)
  x <- as.numeric(x)
  ma <- data.frame(i ,j ,x)
  ma <- ma[order(ma$i, ma$j), ]
  dim_min <- c(max(i), max(j))
  if (missing(dims))
    dims <- dim_min
  else if (!all(dims >= dim_min))
    stop("Dimensions are wrong.")
  else
    dims <- as.integer(dims)
  a <- new("sparse.matrix", i = ma$i ,j = ma$j ,dims = dims, x = ma$x)
  return(a)
}

# Function sparse_add can add two sparse matrices.
sparse_add <- function(e1, e2){
  if (!all(e1@dims == e2@dims))
    stop("Dimentions of two matrices are not identical")
  a <- data.frame(i = e1@i, j = e1@j, x = e1@x)
  b <- data.frame(i = e2@i, j = e2@j, x = e2@x)
  c <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  z <- sparse.matrix(i = c$i, j = c$j, x = c$x, dims = e1@dims)
  return(z)
}

# Set a method "+" using function sparse_add.
setMethod("+", signature(e1 = "sparse.matrix", e2 = "sparse.matrix"), sparse_add)

# Function sparse_multiply can multiply two sparse matrices.
sparse_multiply <- function(x, y){
  if (x@dims[2] != y@dims[1])
    stop("Dimensions of two matrices don't match.")
  dims <- c(x@dims[1], y@dims[2])
  ind <- 0
  for (k in seq_along(x@i))
    for (p in seq_along(y@j))
      if (x@j[k] == y@i[p]){
        ind <- ind +1
        if (ind == 1)
          m <- sparse.matrix(i = x@i[k], j = y@j[p], x = x@x[k]*y@x[p], dims =dims)
        else
          m <- m + sparse.matrix(i = x@i[k], j = y@j[p], x = x@x[k]*y@x[p], dims = dims)
      }
  return(m)
}

# Set a method "%*%" using function sparse_multiply.
setMethod("%*%", signature(x = "sparse.matrix", y = "sparse.matrix"), sparse_multiply)

# Function sparse_tranpose can transpose a sparse matrix.
sparse_tranpose <- function(x){
  dims <- c(x@dims[2], x@dims[1])
  z <- sparse.matrix(i = x@j, j = x@i, x = x@x, dims = dims)
}

# Set a method "t" using function sparse_transpose.
setMethod("t", "sparse.matrix", sparse_tranpose)

