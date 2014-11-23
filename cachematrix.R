## The function makeCacheMatrix creates a list containing functions to
## 1) set the value of the matrix,
## 2) get the value of the matrix,
## 3) set the value of the inverse,
## 4) get the value of the inverse.

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


## The function cacheSolve calculates the inverse matrix using the
## function makeCacheMatrix. If the inverse has already been calculated,
## cacheSolve gets the inverse from the cache and skips the computation.
## If the inverse has not been calculated, cacheSolve calculates
## the inverse and sets the value of the inverse in the cache
## using the set.inverse function.

cacheSolve <- function(x, ...) {
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
