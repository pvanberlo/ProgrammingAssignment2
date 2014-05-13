## The functions in this file can be used to create a 'cached matrix', based on
## a standard R matrix. This cached matrix can be used for quick inversion of
## the matrix (initial call of cacheSolve calls the R solve function and caches
## the result, subsequent calls of the cacheSolve function immediately return
## the cached result).
##
## Example:
##      cm <- makeCacheMatrix(matrix(rnorm(1000000), nrow = 1000, ncol = 1000))
##      cacheSolve(cm)
##

## The makeCacheMatrix function can be used to create a matrix with several 
## additional functions (get, set, setinversion and getinversion). The idea is 
## generally identical to the example in Programming Assignment 2.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversion <- function(inversion) m <<- inversion
    getinversion <- function() m
    list(set = set, get = get, setinversion = setinversion, 
         getinversion = getinversion)
}


## The cacheSolve function is identical to the example in Programming
## Assignment 2, except it does not calculate the mean but rather uses the
## solve function to invert a matrix created by the makeCacheMatrix function.
## This function inverts a square matrix (using solve), and calls the 'cache
## matrix' setinversion function to store the result. A second run on the same 
## 'cache matrix' will return the cached result (getinversion) instead of 
## running the R solve function again.

cacheSolve <- function(x, ...) {
    m <- x$getinversion()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversion()
    m
}
