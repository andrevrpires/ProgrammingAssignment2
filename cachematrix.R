## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.
## 
## These pair of functions are a suggestion about this issue. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## 
## for this purpose, it defines 4 methods:
## to set and get the matrix,
## to set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {str
        
        ## initialize the variable for the inverse of x
        inverse <- NULL
        
        ## this method sets x's value
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## this method returns x's value
        get <- function() x
        
        ## this method sets x's inverse value
        setinverse <- function(solve) inverse <<- solve
        
        ## this method returns x's inverse value
        getinverse <- function() inverse
        
        ## list for method's names resolution
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function works with matrix objects
## created by the function makeCacheMatrix.
## it returns a matrix that is the inverse of 'x'.
## 
## if the inverse of 'x' was already computated once,
## the function gets the inverse from cache
## throught the method from makeCacheMatrix.
## 
## if the inverse of 'x' was not computated yet,
## the function calculates it with the 'solve' base function,
## stores the new matrix on cache, and returns its values.

cacheSolve <- function(x, ...) {
        
        ## try to get inverse from cache
        inverse <- x$getinverse()
 
        ## if the inverse is in cache, returns it
        if (!is.null(inverse)) {
                message("getting inverse from cache")
                return(inverse)
        }
        
        ## if the inverse was not in cache,
        ## gets the data and computes the inverse
        data <- x$get()
        inverse <- solve(data, ...)
        
        ## store the inverse on cache and returns its values
        x$setinverse(inverse)
        inverse
}