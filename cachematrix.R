## The makeCacheMatrix and cacheSolve functions are to be used together to calculate 
## the inverse of a matrix and then cache the results. If the results have already 
## been cached then cacheSolve will not recalculate. 

## makeCacheMatrix will create list of functions 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will calculate the inverse matrix if it has not been calculated yet.
## If it has already been calculated it will retrieve it from cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
