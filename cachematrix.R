## These two functions produce the effect of a matrix that 'remembers' it's own inverse.
## makeCacheMatrix creates a private variable to hold the inverse and returns a list of set and get functions
## cacheSolve can then use these functions to get or set the inverse as required

## makeCacheMatrix sets the cached inverse to null
## and defines set and get functions for the matrix and inverse
## The return value is a named list of these 4 functions

makeCacheMatrix <- function(x = matrix()) {

  # define set and get functions for value and cache
  cacheInverse <- NULL
  set <- function(y) {
    x <<- y
    cacheInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cacheInverse <<- inverse
  getinverse <- function() cacheInverse
  
  # return as a list the functions defined above
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve returns a matrix that is the inverse of 'x'
## If an already cached inverse is found then it is returned
## Otherwise an inverse is calculated using solve()
## This inverse is then cached using setinverse before being returned

cacheSolve <- function(x, ...) {

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
