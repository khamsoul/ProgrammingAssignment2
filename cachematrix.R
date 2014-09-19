## Put comments here that give an overall description of what your
## functions do

## Create a matrix that caches its own inverse to reduce unecessary computation.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL 
    set <- function(y) {
        x <<- y
        ## When setting a new matrix, the previous inverse is now invalid and null
        inverse <<- NULL 
    }
    get <- function() x
    setInverse <- function(inputInverse) inverse <<- inputInverse
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Takes a CacheMatrix object and returns its inverse from cache, or calculates 
## it and stores it if none exists yet.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <-solve(data, ...)
    x$setInverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
