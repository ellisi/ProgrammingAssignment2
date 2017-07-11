## The functions makeCacheMatrix and cacheSolve work together to
## compute the inverse of a matrix, but if the inverse calculation
## has already been previously performed, it is just retrieved from
## the cache rather than recalculating

## The function makeCacheMatrix creates a special matrix object
## that caches its inverse, so that the same inverse calculation
## doesn't have to be repeated later if the inverse is already
## available in the cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The function cacheSolve inverts a matrix object as created by the
## makeCacheMatrix function.  The inverse calculation isn't repeated if
## the same inverse is already available in the cache, it's just
## retrieved from there.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
