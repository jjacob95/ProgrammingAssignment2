## cacheMatrix is a matrix object for efficiently retrieving the inverse of a matrix. the inverse is computed
## once (the first time the inverse is requested) and cached for future use

## Function to create a matrix object with cached inverse

makeCacheMatrix <- function(x = matrix()) {
    xI <- NULL
    set <- function(y) {
        x <<- y
        xI <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) xI <<- inv
    getInverse <- function() xI
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Cached version of matrix inversion function - calculates and caches the inverse on first invocation,
## returning the cached value on subsequent calls

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xI <- x$getInverse()
    if (!is.null(xI)) {
        message("Using cached value of inverse matrix")
        return(xI)
    }
    xData <- x$get()
    xI<-solve(xData)
    x$setInverse(xI)
    xI
}

