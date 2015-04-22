## These functions can be used to cache the inverse of a matrix.
## makeCacheMatrix is used to manage and initialized an object with attributes:
## 1. the matrix where we have to compute the inverse
## 2. the inverse matrix initialized with a NULL value
## cacheSolve return the inverse of a given makeCacheMatrix object in two ways:
## 1. hit the cached value in the inverse attribute and return it
## 2. if the value was not already calculated, compute the inverse using solve
##    function and save it into inverse attribute for next use.

## Inizialize an object containing the matrix and the inverse as initial
## value NULL.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve try to get the inverse from cached data.
## If there is no caching data calculate the inverse of the matrix using
## solve function and then save the result in the object for next call of
## cacheSolve function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # Compute the inverse if data is not cached
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

# Example of use (Uncomment next lines)
# X = matrix(1:4, 2,2)
# matrix <- makeCacheMatrix(matrix(1:4, 2,2))
# cacheSolve(matrix)
# cacheSolve(matrix)
