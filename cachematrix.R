## R function to cache potentially time-consuming computations

## DESCRIPTION: This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## DESCRIPTION: This function calculates the inverse of the matrix created by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), it will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
   inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
