## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a special "vector",
## which is a list containing a function to 1) set the value of the matrix,
## get the value of the matrix, set the value of the inverse,
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set.inverse <- function(solve) i <<- solve
  get.inverse <- function() i
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## The function cacheSolve calculates the inverse matrix created with the
## function makeCacheMatrix. If the inverse has already been calculated,
## it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value
## of the inverse in the cache via the set.inverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$get.inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set.inverse(i)
  i
}
