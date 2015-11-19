## The function takes the inverse of a matrix.
## In order to optimize performance, it stores the value of the inverse for any invertible matrix entered
## If the same matrix is called again, the program will not calculate it one more, but show the stored result.

## This function calculates the inverse of any invertible matrix and puts it in cache.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

## This function return a matrix that is the inverse of 'x', either from the cache or by calculating it.
cacheSolve <- function(x=matrix(), ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matriz <- x$get()
  m <- solve(matriz, ...)
  x$setinverse(m)
  m
}
