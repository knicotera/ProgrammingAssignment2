## These functions (makeCacheMatrix and cacheSolve) cache the inverse of a 
## matrix in order to avoid the costly computation times often attributed to 
## matrix conversion.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## It sets the value of the matrix, gets the value of the matrix, sets the value 
## of the inverse, and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, get=get, 
         setInverse=setInverse, 
         getInverse=getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
    return(inv)
}