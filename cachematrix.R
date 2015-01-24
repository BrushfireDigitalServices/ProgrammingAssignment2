## The functions 'makeCacheMatrix()' and 'cacheSolve()' are designed to save 
## computation when the inverse of a given matrix must be retreived repeatedly 
## (such as in a loop). 
##
## The advantage of using these functions is threefold:
##    1. Rather than simply saving the inverse outside the loop, these 
##       functions track changes and can trigger recalculation of the inverse.
##    2. More than one closure can be in memory at the same time and behave 
##       totally independent of one another, even if they were constructed from
##       the same underlying data.
##    3. The underlying data sets can be modified without interfering with
##       the caching closures the data sets were used to construct.
## 
## makeCacheMatrix(x) can be



## Function: makeCacheMatrix(x = matrix())
# 
## Factory to create and return a "special matrix" (in the form of a list of 
## functions defined below) which can save computation by caching its inverse. 
##
## Argument 'x' may be a matrix, or a data type which will be coerced to matrix.
##
## Usage: Always save the output of this function to a name. For example,
##        if 'x' is an invertible martix, 'cacheMatrix <- makeCacheMatrix(x)'
##        will enable repeated access to the caching and matrix modification 
##        features because the name 'cacheMatrix' will point to the same closure.
##
## Because makeCacheMatrix returns closures containing copies of the supplied 
## data, more than one such object can be in memory at any given time without 
## interference.
makeCacheMatrix <- function(x = matrix()) {
    
    ## The "cached" inverse of the matrix 'x' or NULL.
    ## NULL acts as a flag that inverse must be calculated.
    inv <- NULL
    
    ## Function: setMatrix(y)
    ##
    ## The "original" matrix will be set to 'y'.
    ## The cached inverse will be flushed by setting 'inv' to NULL.
    ##
    ## NOTE: "Lazy" principles are preserved (esp. for large datasets)
    ## by not performing any correctness checks on whether or not matrix 'y'
    ## can be inverted.
    ##
    ## This behavior is AS SPECIFIED at README.md line 86:87
    ## which reads "For this assignment, assume that the matrix supplied is
    ## always invertible."
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Function: getMatrix()
    ##
    ## Returns the "original" matrix.
    ## Will return either:
    ##  - the matrix passed to the most recent call to setMatrix,
    ##  - the original argument 'x' passed to makeVector if and
    ##    only if setMatrix has not yet been called on this closure.
    ##
    ## NOTE: If the returned matrix is assigned to a variable 'foo',
    ## modifications to 'foo' will not affect 'x', and vice-versa.
    getMatrix <- function() 
        x
    
    ## Function: setInverse(newInv)
    ##
    ## Used by the cacheSolve function to set the value of the 
    ## cached matrix to the inverse of the "original" matrix
    ## 
    ## NOTE: "Lazy" principles are preserved (esp. for large datasets)
    ## by not performing any correctness checks on 'newInv'
    ## before assigning the value of 'newInv' to 'inv'
    setInverse <- function(newInv) 
        inv <<- newInv
    
    ## Function: getInverse()
    ##
    ##
    getInverse <- function() 
        inv
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'. Specifically, x must be 
## a specially-crafted list such as returned by makeCacheMatrix(x = matrix()).
##
## Usage: if 'x' is an invertible martix and 'cacheMatrix <- makeCacheMatrix(x)'
##        call 'cacheSolve(cacheMatrix, ...)'
##
## The "..." in the argument list will be passed to the 'solve' function.
## Run '?solve' for more details.
## 
## Caching feature requires the same list environment every time. Recall that  
## if the matrix within the list has changed, its inverse will be recalculated.
## Therefore, repeatedly calling cacheSolve(makeCacheMatrix(x)) 
## will NOT allow cache access.
cacheSolve <- function(x, ...) {
    
    ## Gets the cached value of inverse matrix from 'x'
    ## and saves it to local name 'm'.
    m <- x$getInverse()
    
    ## When m is not null, the cached matrix is assumed to be accurate 
    ## and is returned, causing cacheSolve to exit.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## All code below is effectively an "else" block because of the return in  
    ## the "if" above. Will only execute if 'm' is null, signalling no cached 
    ## inverse exists.
    ## An inverse will now be calculated, cached, and returned.
    
    ## First, the "original" matrix is retreved and saved to the local name 
    ##        'data' for clarity.
    data <- x$getMatrix()
    
    ## Next, the "inverted" matrix is calculated and 'm' is reset to its value.
    ##       This is also done for rclarity.
    m <- solve(data, ...)
    
    ## If execution has reached this point, 'm' is the inverse of 'x' and should 
    ## be cached.
    x$setInverse(m)
    
    ## Because this line is the last to be evaluated in cacheSolve(), it is 
    ## "effectively" the same as 'return(m)'. The matrix m returned.
    m
}
