## Functions to cache the inverse of a matrix

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # Initializing the inverse property
        
        # Matrix being set
        set <- function(y){ 
                x <<- y
                i <<- NULL
        }
        # Getting the matrix
        get <- function() x
        
        # Setting the matrix inverse
        setInv <- function(inverse) i <<- inverse
        
        # Getting the matrix inverse
        getInv <- function() i
        
        #Return list of methods
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## Compute inverse of matrix returned by "makeCacheMatrix". If already calculate, "cacheSolve" retrieves inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInv()
        
        # Return inverse if it's already set
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # Get matrix from our object
        data <- x$get()
        
        # Calculate inverse with matrix mult.
        i <- solve(data, ...)
        
        # Set inverse to object
        x$setInv(i)
        
        # Return matrix
        i
}
