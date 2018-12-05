#' Create a new class called "sparse.matrix", and define addition, multiplication, and transpose of it. 
#'
#' @description I set a new class called "sparse.matrix", and created a function also called 
#' sparse.matrix to return a "sparse.matrix" object. Then, I created functions sparse_add, 
#' sparse_multiply and sparse_tranpose to do sparse matrix operations. At last, I set methods 
#' "+", "%*%" and "t" by using the three functions.
#' @param i a vector indicating row number with non-zero elements
#' @param j a vector indicating columnn number with non-zero elements
#' @param x a vector indicating the value of each non-zero element
#' @param dims a vector indicating dimensions of the matrix
#' @param e1 a sparse.matrix
#' @param e2 a sparse.matrix
#' @param x a sparse.matrix
#' @param y a sparse.matrix
#' @return A sparse.matrix object
#' @importFrom methods new
#' @examples
#' sm <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(1, 1), dims = c(2, 2))
#' sm
#' sm + sm
#' sm %*% sm
#' t(sm)
#' @export
setClass("sparse.matrix", representation(i = "integer", j = "integer", dims = "integer", x = "numeric"))
#' @export
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
#' @export
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
#' @export
setMethod("+", signature(e1 = "sparse.matrix", e2 = "sparse.matrix"), sparse_add)
#' @export
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
#' @export
setMethod("%*%", signature(x = "sparse.matrix", y = "sparse.matrix"), sparse_multiply)
#' @export
sparse_tranpose <- function(x){
  dims <- c(x@dims[2], x@dims[1])
  z <- sparse.matrix(i = x@j, j = x@i, x = x@x, dims = dims)
}
#' @export
setMethod("t", "sparse.matrix", sparse_tranpose)