## These functions cache the inverse of a matrix to avoid repeated
## computationally expensive inversions.

## 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinverse <- function() inv
  
  # Return list of internal functions
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and matrix hasn't changed), 
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # Check if inverse is already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Compute inverse if not cached
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  # Return the computed inverse
  inv
}
