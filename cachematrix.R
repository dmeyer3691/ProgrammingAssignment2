## These functions can be used to store a cache of the inverse of a matrix.
## This is useful if the matrix is large and the computation will take place multiple times.
## No input validation or error checking take place.


## This function creates a list that contains four functions.
## set(y) will set the stored matrix to y and reset the inverse to NULL
## get() will return the stored matrix
## setInverse(theInverse) will cache the inverse of the matrix x
## getInverse() will return the cached inverse of the matrix x
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(theInverse) inverse <<- theInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## This function returns the inverse of a matrix x
## Input value x is the output of makeCacheMatrix from above (ie a list)
## If the input x already has a cached version of the inverse of the stored matrix,
## then it outputs this before doing any computations. If the inverse of the stored
## matrix is not cached already, this function will compute the inverse and cache
## the inverse in x.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
