## Programming Assignment 2
## By: Carlos Patino
## 19-MAR-2015
## This script contains the functions needed to cache the inverse of a given
## matrix. 

## makeCacheMatrix creates a special object containing the set of functions 
## that set and get the input matrix and its inverse. 
## The input should be a square (invertible) matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of the special matrix object
## returned by makeCacheMatrix(x).
## If x has not changed, and its inverse is cached, then this function returns
## the cached inverse of matrix x

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv ## returns inverse of matrix stored in x.
}