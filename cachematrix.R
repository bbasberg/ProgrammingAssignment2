## In compute-intensive operations, it’s beneficial to be able to
## cache time-consuming computations.  If we can reuse the cache the results of the 
## computation then subsequent requests for the results can be retrieved from the cache, ## assuming the matrix hasn’t changed, rather than recalculated.

## The makeCacheMatrix function creates a special “matrix” object that can cache its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function computes the inverse of a special “matrix” that’s returned
## by the makeCacheMatrix function
## Assume that the matrix supplied is always invertible for this project

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  iv <- x$getinverse()
  if(!is.null(iv)) {
    message("getting cached data.")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data)
  x$setinverse(iv)
  iv
}
