## The purpose of these functions is to cach the inverse of a matrix 
## so when we need it again, it can be looked up in the cache rather than recomputed. 
## The makeCacheMatrix function will cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## The cacheSolve fuction function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix. 
#If the inverse has already been calculated (and the matrix has not changed)
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
  
}
