## Compute the inverse of a matrix, using cache to avoid recomputing the inverse
## when the contents of the matrix do not change.
 
## Given a matrix, the first function 'makeCacheMatrix' gives a vector of functions 
## required to set and get both the matrix and its inverse. 

## Given the vector created by 'MakeCacheMatrix', the second function, 'cacheSolve'
## computes the inverse of the matrix or gives the cached inverse if the matrix was
## already used in a previous inverse computation.

## Function makeCacheMatrix
##=========================

## This function 'makeCacheMatrix' creates a special "vector", which is
## a list containing four functions to: 
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
           x <<- y
           inv <<- NULL
    }
    get <- function() x
    setinv <- function(invmatrix) inv <<- invmatrix
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}  


## Function cacheSolve
##======================

## This function calculates the inverse of a matrix, using the special
## vector of functions created with 'makeCacheMatrix'. 
## This function first checks to verify if the inverse has already been calculated.
## If so, the 'getinv' function gets the mean from the cache and skips the computation
## Otherwise, it calculates the inverse of the matrix, and set the value of the inverse in the cache via the 'setinv' function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
         message("getting cached data")
         return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

