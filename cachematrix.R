
## --------------------------------------------
## Assignment: Caching the Inverse of a Matrix
## Author: Minerva Enriquez
## --------------------------------------------

## 1.makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

## 2. cacheSolve: 
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## Assumptions: Assume that the matrix supplied is always invertible
## Usage: (with sample code from 'solve' function )
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h5 <- hilbert(5); h5
## ch5<-makeCacheMatrix(h5)
## cacheSolve(ch5)
## cacheSolve(ch5)

makeCacheMatrix <- function(x = matrix()) {
    ## if x is a square invertible matrix
    ## then solve(x) which is its inverse

    r <- NULL
    # set is a function that changes the matrix stored in the main function #
    set <- function(y) {
        x <<- y
        r <<- NULL
    }
  
    # Returns the matrix stored in the main function #
    get <- function() x   
    
    # function that store the value of the input in a variable "r" into
    # the "main" function.
    setsolve <- function(solve) r <<- solve

    # functions that return the value of the input in a variable "r" from
    # the "main" function.
    getsolve <- function() r

    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
    r <- x$getsolve()
    if(!is.null(r)) {
        message("getting cached data")
        return(r)
    }
    data <- x$get()
    r <- solve(data, ...)
    x$setsolve(r)
    r 
}
