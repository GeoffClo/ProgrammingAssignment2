## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  cacheInverse <- NULL
  set <- function(y) {
    x <<- y
    cacheInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cacheInverse <<- inverse
  getinverse <- function() cacheInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    cacheInverse <- x$getinverse()
    if(!is.null(cacheInverse)) {
      message("getting cached data")
      return(cacheInverse)
    }
    data <- x$get()
    cacheInverse <- solve(data, ...)
    x$setinverse(cacheInverse)
    cacheInverse
  }
