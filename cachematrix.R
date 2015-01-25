## Creates a special "matrix" object that can cache its inverse.
## This object is represented as a list of functions.
##
## Args:
##   x: matrix.
##
## Returns:
##   The "matrix" object which is just a list of functions.
makeCacheMatrix <- function(x = matrix()) {
    
    ## Variable for caching an inverse of the matrix x.
    ## Initially set to NULL.
    inv <- NULL
    
    ## Stores new matrix.
    ## Resets cached inverse to NULL.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Gets stored matrix.
    get <- function() x
    
    ## Stores inverse in cache.
    setInverse <- function(inverse) inv <<- inverse
    
    ## Gets cached inverse.
    getInverse <- function() inv
    
    ## returning the list of described functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix(). It assumes, that the "matrix" 
## is not NULL and contains not NULL R matrix inside. Also, the inverse 
## should exist for this underlying R matrix.
## It first checks a cache of the "matrix" and if the cache is not empty
## returns cached value. Other way, it returns the calculated value 
## and sets the cache to this value.
##
## Args:
##      x: Special "matrix" which can be constructed by 
##          makeCacheMatrix() function.
##   ... : other argumets which all are passed to the underlaying call 
##          to the solve() function immediately after its first argument.
##
## Returns:
##   The inverse of the matrix if it exists.
cacheSolve <- function(x, ...) {
    
    ## Getting an inverse from the "matrix" cache.
    inverse <- x$getInverse()
    
    ## Checking that cached inverse exists.
    if(!is.null(inverse)) {
        message("getting cached data")
        
        ## Returning the cached inverse.
        return(inverse)
    }
    
    ## Cached inverse doesn't exist. Calculating it.
    
    ## Getting the matrix.
    data <- x$get()
    
    ## Calculating the inverse.
    inverse <- solve(data, ...)
    
    ## Setting the inverse to the matrix cache.
    x$setInverse(inverse)
    
    ## Returning the calculated inverse.
    inverse
}
