## This file contains 2 functions -> 1. makeCacheMatrix
## 2. cacheSolve. These functions demonstrate the use of the variables (objects)
## stored and accessible outside the function


#' The function sets a vector using the superassignment operator
#' thereby making it available outside the function.
#'  
#' @param x A matrix.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' makeCacheMatrix(matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE))
makeCacheMatrix <- function(x = matrix()) {
  inverseX<<-NULL
  set <- function(y) {
    x <<- y
    inverseX<<-NULL
  }
  get <- function() x
  setInverse<-function(inverse)inverseX <<-inverse
  getInverse<-function()inverseX

  list(set = set, get = get, setInverse=setInverse, getInverse=getInverse)
}


#' The function determines if the vector's inverse has been calucalated by
#' accessing the common object stored outside the scope of the function.
#' If the inverse has been calculated, it returns it
#' If not, it calculates the inverse, updates the cache and returns the matrix
#'  
#' @param x A matrix.
#' @return The inverse of the input matrix
#' @examples
#' cacheSolve(a)

cacheSolve <- function(x, ...) {
  
  mat <- x$get()
  ## Get the inverse from the variable stored in a broader scope
  matInverse <-x$getInverse()
  ## Check if cache is populated and inverse is calculated
  if(!is.null(matInverse)) {
    message("getting cached data")
    return(matInverse)
  }
  data <- x$get()
  ## Calculate the inverse
  inverseX <- solve(data, ...)
  x$setInverse(inverseX)
  ## Return a matrix that is the inverse of 'x'
  inverseX
}

