## Caching the Inverse of a Matrix

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    get <- function() x
    setinverse <- function (inverse) z <<- inverse
    getinverse <- function () z
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function returns the inverse of the matrix computed by the 
## makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
    z <- x$getinverse()
    if (!is.null(z)) {
        message("getting cached data"")
        return z
    }
    matrix.data <- x$get()
    z <- solve(matrix.data,...)
    x$setinverse(z)
    return z
}
