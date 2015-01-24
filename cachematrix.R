## Put comments here that give an overall description of what your
## functions do

## Factory to create and return a "special matrix"
##
##
##
makeCacheMatrix <- function(x = matrix()) {
    
    ## The "cached" inverse of the matrix 'x' or NULL.
    ## NULL acts as a flag that inverse must be calculated.
    inv <- NULL
    
    ## The "un-inverted" matrix will be set to 'y'.
    ## The cache will be flushed by setting 'inv' to NULL
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Returns the "un-inverted" matrix.
    ## Will return either:
    ##  - the matrix passed to the most recent call to setMatrix,
    ##  - the original argument 'x' passed to makeVector if and
    ##    only if setMatrix has not yet been called on this closure.
    ##
    ## NOTE: If the returned matrix is assigned to a variable 'foo',
    ## modifications to 'foo' will not affect 'x', and vice-versa.
    getMatrix <- function() 
        x
    
    ## Used by the cacheSolve function to set the value of the 
    ## cached matrix to the inverse of the "un-inverted" matrix
    ## 
    ## NOTE: "Lazy" principles are preserved (esp. for large datasets)
    ## by not performing any correctness checks on 'newInv'
    ## before assigning the value of 'newInv' to 'inv 
    setInverse <- function(newInv) 
        inv <<- newInv
    
    getInverse <- function() 
        inv
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getMatrix()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
