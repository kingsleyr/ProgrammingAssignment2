## 2 functions for caching and inverting a matrix


## makeCacheMatrix creates a matrix object that can cache its inverse
## cacheSolve computes the inverse of the makeCacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
  
  #initialize inverted matrix
  inverted.matrix <- NULL
  
  # set matrix to cache, superassignment required
  set <- function(y) {
    x <<- y
    inverted.matrix <<- NULL
  }
  
  get <- function() x
  
  # use solve f'n to get 
  set.inverse <- function(solve) inverted.matrix <<- solve
  get.inverse <- function() inverted.matrix
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}

## use the cached matrix, calculate inverse returned by makeCacheMatrix
## If the inverse is already calculated then cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
  inverted.matrix <- x$get.inverse()
  # check if inverted matrix already exists
  if(!is.null(inverted.matrix)) {
    message("Retrieving already cached inverted matrix")
    return(inverted.matrix)
  }
  # Compute inverse if inverted.matrix not already in cache
  data <- x$get()
  inverted.matrix <- solve(data)
  x$set.inverse(inverted.matrix)
  inverted.matrix
  
}
