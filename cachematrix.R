## The following two functions provides basic functionality of caching the invserse of a
## given matrix. First call to cacheSolve computes the inverse of the matrix and returns
## the result. While the subsequent runs to cacheSolve for the identical matrix returns 
## results from the stored cache.

## makeCacheMatrix creates a list of functions to,
# 1. set the value of given matrix.
# 2. get the value of given matrix.
# 3. set value of inverse of given matrix.
# 4. get value of inverse of given matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve checks the cache for computed value of the inverse matrix.
# If a computed value is stored in cache, it is returned using the getinv().
# Else if the cached value for given matrix is not found, a new value for inverse of given 
# matrix is computed using solve() and stored in cache using setinv() for later use.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
