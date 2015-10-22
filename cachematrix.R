## The following functions create an interface to a "matrix" object that caches
## its own inverse. After creating or re-setting the value of the matrix
## object, the first call to retrieve the inverse will calculate
## the inverse of the matrix. Subsequent calls with retrieve the cached
## value.

## The makeCacheMatrix function will create the matrix object interface.
## The function argument x should be an invertible matrix.
## The caller should use the set function to change the underlying matrix
## object to a different invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function calculates, if necessary, and returns the 
## inverse of the underlying matrix object stored in the makeCacheMatrix
## matrix interface.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
