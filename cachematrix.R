## The overall functions will cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {
    x
  }
  setSolve <- function(solve) {
    i <<- solve
  }
  getSolve <- function() {
    i
  }
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## (If the inverse has already been calculated and the matrix has not changed, then the cachesolve will retrieve the inverse from the cache.)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getSolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setSolve(i)
  i
}
