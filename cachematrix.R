## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
    x <<- y
    i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
  }
## Write a short comment describing this function
# Matrix inversion is usually a costly computation and it might be useful to
# cache the inverse of a matrix rather than compute it repeatedly. The
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
        ## Return a matrix that is the inverse of 'x'