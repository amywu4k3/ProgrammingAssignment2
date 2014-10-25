## Matrix inversion is usually a costly computation and 
## their may be some benefit to caching the inverse of 
## a matrix rather than compute it repeatedly (there are
## also alternatives to matrix inversion that we will not
## discuss here). Your assignment is to write a pair of
## functions that cache the inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Set the inverse to NULL
  i <- NULL
  
  # Set function to
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Get function
  get <- function() x
  
  # SetInverse
  setInverse <- function(inverse) i <<- inverse
  
  # GetInverse
  getInverse <- function() i
  
  # List
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve
## the inverse of from the cache.

cacheSolve <- function(x, ...) {
  ## Get exisiting inverse
  i <- x$getInverse()
  
  ## if it NOT null, return value
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  ## Generate an inverse
  data <- x$get()
  i <- solve(data)
  
  ## Set the calculated inverse into cache
  x$setInverse(i)
  
  ## Return the inverse
  i
}
