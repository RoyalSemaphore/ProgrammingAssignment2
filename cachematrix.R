## This pair of functions allows you to cache the inverse of a matrix
## 
## Usage:
## m<-matrix(1:4, nrows=2, ncols=2)
## e<-MakeCacheMatrix(m)
## CacheSolve(e)
## CacheSolve(e) again...reports that it obtained the inverse (solve) from cache

## Creates a special "matrix" object that can cache its inverse.

MakeCacheMatrix <- function(x = numeric()) {
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


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.

CacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}