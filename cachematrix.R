## Inverting a matrix is CPU bandwidth intensive 
## These methods cache results so that repeated use
## saves time and CPU bandwidth.

## Special matrix object that provides functions for caching 
## an inverted matrix.  A user wishing to cache matrix inversions
## creates this object and passes it to the casheSolve function.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvert <- function(solve) m <<- solve
    getinvert <- function() m
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## The cacheSolve is used to invert a square matrix and also cache 
## the value to save time for future uses.  This function requires the 
## special matrix object returned from makeCacheMatrix.
cacheSolve <- function(x, ...) {
    m <- x$getinvert()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m
}
