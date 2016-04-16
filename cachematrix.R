## This code includes two functions. The first one creates a special object that stores
## a matrix. The second one caches the inverse of the matrix created by the first
## function.

## Function to create an object x that stores a matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(z) {
      x <<- z
      s <<- NULL
      }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function to calculate the inverse of the matrix created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
      message("getting cached data")
      return(s)
      }
  matdata <- x$get()
  s <- solve(matdata, ...)
  x$setsolve(s)
  s
}