## The following functions cache the inverse of a matrix
## We assume that the matrix is invertible

## makeCacheMatrix creates a special matrix that can chache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get matrix
  get <- function() x
  ## set the inverse of the matrix
  setinverse <- function(solve) inv <<- solve
  ## get the inverse of the matrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates inverse of special matrix created by makeCacheMatrix
## If the inverse is already calculated, get it from the cache 
## If not, calculate the inverse and set it in the cache 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  ## Check if inverse is in cache
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## Inverse is not in cache, calculate the inverse
  data <- x$get()
  inv <- solve(data,...)
  ## set inverse in the cache
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
