# Matrix inversion is usually a costly computation and there may be some benefit to caching
# the inverse of a matrix rather than compute it repeatedly (there are also alternatives to
# matrix inversion that we will not discuss here). Your assignment is to write a pair of functions
# that cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse_input) inverse <<- inverse_input
  get_inverse <- function() inverse
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("returning cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}

# quick tests
# m <- matrix(1:4, 2, 2)
# mcm <- makeCacheMatrix(m)
# cacheSolve(mcm)
# cacheSolve(mcm)
