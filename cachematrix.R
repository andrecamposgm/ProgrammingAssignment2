## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#' This function creates a special "matrix" object that 
#' can cache its inverse.
#' 
#' @param x A matrix that will be cached
#' @return The special matrix that will be cached when the cacheSolve is used
#' 
makeCacheMatrix <- function(x = matrix()) {
  # let's create an internal cache to save the inverse of 
  # the matrix
  internalCache <- NULL
  set <- function(y) {
    # humm ... the matrix has been changed, let's discard the cache
    # and hope some day some one will need it to be calculated again! 
    x <<- y
    internalCache <<- NULL
  }
  get <- function() x
  setInverse <- function(calculatedInverse) internalCache <<- calculatedInverse
  getInverse <- function() internalCache
  list(
    set = set,
    get = get, 
    setInverse = setInverse, 
    getInverse = getInverse
  )
}


## Write a short comment describing this function
#' This function computes the inverse of the special "matrix" 
#' returned by makeCacheMatrix. If the inverse has already 
#' been calculated (and the matrix has not changed), then 
#' cacheSolve should retrieve the inverse from the cache.
#' 
#' @param x The special matrix created using the makeCachedMatrix. 
#' @return The inverse of the matrix. It makes sure to cache internally in the object
#'  before returning it
#' 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  internalCache <- x$getInverse()
  # is the matrix already cached? 
  if(!is.null(internalCache)) {
    # great! not necessary to recalculate it! :D 
    message("Getting Inverse From Cache")
    return(internalCache)
  }
  # oh no! we will need to calculate it! 
  data <- x$get()
  internalCache <- solve(data,...)
  x$setInverse(internalCache)
  internalCache
}
