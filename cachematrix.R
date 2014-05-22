## a set of functions to solve the inverse of a matrix and
## cache the result.

## makeCacheMatrix caches a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(inv) m <<- inv
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}


## cacheSolve takes as input a matrix and computes its inverse
## if the inverse has been solved this returns the cached result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m
}
