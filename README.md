setwd('C:/Users/rubind1/Documents/Coursera-R')
makeCacheMatrix <- function(x = matrix()) {
  # todo error if x is not a matrix
  u <- NULL
  set <- function(y) {
    x <<- y
    u <<- NULL
  }
  get <- function() x
  setinverse <- function(u) inv <<- u
  getinverse <- function(u) u
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' Compute and cache the inverse
## Same here, changed "mean" to "solve" and "m" to "s"
#' x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' cacheSolve(x)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  u <- x$getinverse()
  if(!is.null(u)) {
    message("getting cached matrix inverse")
    return(u)
  }
  data <- x$get()
  u <- solve(data, ...)
  x$setinverse(u)
  u
}
