#
## Cache the inverse of a matrix inside an object.
#

#
## makeCacheMatrix constructs an object that stores a matrix and 
## its inverse (lazily evaluated by cacheSolve).
#

rm(list=ls())

makeCacheMatrix <- function(x=matrix()) {
  # Initialize the cached data.
  data <- x
  # Initialize the cached result to NULL (a flag).
  result <- NULL
  # Update the cached data and reset the cached result.
  set <- function(y) {
    # Cache the data.
    data <<- y
    # Reset the cached result to NULL (a flag).
    result <<- NULL
  }
  # Get the currently cached data.
  get <- function() data
  # Update the result cache.
  setresult <- function(z) result <<- z
  # Get the currently cached result.
  getresult <- function() result
  # Generate the cached object for x.
  list(set=set, get=get,
       setresult=setresult,
       getresult=getresult)
}

#
## cacheSolve operates on a makeCacheMatrix object, returning its inverse.
## On the first call, the inverse is calculated, cached and returned.
## On subsequent calls the cached value is returned.
#
cacheSolve <- function(x, ...) {
  # Cached?
  result <- x$getresult()
  if(!is.null(result)) {
    message("returning cached result")
    return(result)
  }
  # Calculate, and remember, a result from the cached data.
  message("calculating a result from cached data")
  y <- x$get()
  z <- solve(y, ...)
  x$setresult(z)
  z
}


"

a <- matrix(c(2, 3, 2, 2), nrow=2, ncol=2)
a
cma <- makeCacheMatrix(a)
cma
cacheSolve(cma)
cacheSolve(cma)
cma$setresult(NULL)
cacheSolve(cma)
cacheSolve(cma)

b <- matrix(c(1, 1, -1, 2), nrow=2, ncol=2)
b
cmb <- makeCacheMatrix(b)
cmb
cacheSolve(cmb)
cacheSolve(cmb)
cmb$setresult(NULL)
cacheSolve(cmb)
cacheSolve(cmb)

cacheSolve(cma)
cacheSolve(cmb)

z <- matrix(c(11, 22, 33, 44), nrow=2, ncol=2)
z
cma <- makeCacheMatrix(z)
cma
cacheSolve(cma)
cacheSolve(cma)

"


