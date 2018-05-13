## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(v_inverse) inv_matrix <<- v_inverse
  getInverse <- function() inv_matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getInverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  mymat <- x$get()
  inv_matrix <- solve(mymat, ...)
  x$setInverse(inv_matrix)
  inv_matrix
}
