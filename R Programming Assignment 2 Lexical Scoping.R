## makeCacheMatrix catches cache in its inverse

## The makeCacheMatrix have the set, get, setinv, getinv to get the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) {inv <<- inverse}
    getinverse <- function() {inv}
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }




## This is used to get the cache data
## And to check whether inverse in NULL

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

