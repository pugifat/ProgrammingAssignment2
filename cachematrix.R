## Contains functions to create a special matrix with cache-able innverse
## and compute function to resolve and cache the inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## initializing inverse cache
    m <- NULL

    ## defining set function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    ## defining get function
    get <- function() x

    ## defining setInverse
    setInverse <- function(inverse) m <<- inverse

    ## defining getInverse
    getInverse <- function() m

    ## returning list of available internal functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. ## If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()

    ## returning cached data if exists
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    ## computing and caching inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)

    ## Return a matrix that is the inverse of 'x'
    m
}
