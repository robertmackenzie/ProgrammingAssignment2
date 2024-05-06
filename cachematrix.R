## cachematrix.R provides a matrix implementation with a memoised invert operation

## Construct a "CacheMatrix" object

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(get = get, set = set,
        getInverse = getInverse, setInverse = setInverse)
}


## Invert a "CacheMatrix" object, memoising the result

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
      print("using cached result")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
