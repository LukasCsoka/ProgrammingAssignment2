## Calculating inverse of matrix is computational intensive. Function makeCacheMatrix create an R object, that caches the inverse of matrix.



## makeCacheMatrix creates an R object, that caches the inverse of matrix calculated by function cacheSolve.
## It takes one optional parameter - matrix m.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse=setInverse, getInverse = getInverse)
}


## Function cacheSolve return a cached inverse of matrix, if exist. 
## If doesn't exist, it computes inverse, save it in cache and return inverse of matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
