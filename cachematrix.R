## Creats an object containing the matrix x and a cache 
## which can be used to store the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Retruns the inverse of the matrix part of the cached object
## x. It first checks to see if the inverse has alrady been computed.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
   ## Return a matrix that is the inverse of 'x', and stores
  ## the inverse in the cached object.
}
