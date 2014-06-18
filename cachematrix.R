## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function( mat ) m <<- mat
  getcache <- function() m
  list( set = set, get = get, setcache = setcache, getcache = getcache )
}


## This function computes the inverse of the special 'matrix' returned by makeCacheMatrix
## above. If the inverse has already been computed then the cachesolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve( mat, ...)
  x$setcache( m )
  m
}
