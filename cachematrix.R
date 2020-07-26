## makeCacheMatrix && cacheSolve functions

## This function sent information to cache

makeCacheMatrix <- function(x = matrix()) {
	a <- NULL
  	set <- function(y) {
          x <<- y
          a <<- NULL
  	}
  	get <- function() x
  	setinverse <- function(inverse) a <<- inverse
  	getinverse <- function() a
  	list(set = set,
       	get = get,
       	setinverse = setinverse,
      	 getinverse = getinverse)
}


## cacheSolve allows to recuperate values from cache

cacheSolve <- function(x, ...) {
	a <- x$getinverse()
  if (!is.null(a)) {
          message("getting cached data")
          return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}
