## This script contains functions for creating matrices and calculating
## inverse matrices, it combination with caching capabilities.

## Creates a cachable matrix with getters and setters for the inputmatrix
## and its calculated inverse.

makeCacheMatrix <- function(x) {
  i <<- NULL
  # function to set the original matrix.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # function to retrieve original matrix.
  get <- function () x
  # function to set the inverse of the original matrix.
  setInv <- function (inverse) i <<- inverse
  # function to retrieve the inverse of the original matrix.
  getInv <- function () i
  
  list(
      set = set, 
      get = get,
      setInv = setInv,
      getInv = getInv
      )  
}

## function for calculating the inverse of the matrix, 
## only calculates the inverse if no value found in cache.

cacheSolve <- function(x, ...) {
  # calls getInv and stores the value in i.
  i <- x$getInv()
  # checking if there is an inverse matrix availble, returning 
  # it when there is one.
  if (!is.null(i)) {
    return(i)
  }
  # calculating the inverse, setting it and returning it if there is 
  # no inverse matrix available in cache.
  else {
    data <- x$get()
    i <- solve(data)
    x$setInv(i)
    return(i)
  } 
}
