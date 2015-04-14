## This R-script contains two functions (i.e. makeCacheMatrix and cacheSolve).
## These scripts can be use to cache the inverse of a matrix, with the purpose
## of using the inverse of the matrix again, without calculating it again. 


## The makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    # set the initial value the inversed matrix (cache) to NULL
    cache <- NULL 
    
    # cache the matrix and set the cached inverse matrix to NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    # get the matrix from the cache
    get <- function() x
    
    # cache the inverse matrix
    setInverse <- function(solve) cache <<- solve
    
    # get the inverse matrix from the cache
    getInverse <- function() cache
    
    # return a list of all the elements in the function
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    
    # get the cached inverse matrix
    cache <- x$getInverse()
    
    # if a cached inverse matrix exists, return the cached inverse matrix
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    
    # if it does not exist, determine the inverse matrix, cache it and return it
    data <- x$get()
    cache <- solve(data, ...)
    x$setInverse(cache)
    return(cache)
}
