##  These pair of functions can cache the inverse of a matrix. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function computes and returns the inverse of the special 
## "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i

}

