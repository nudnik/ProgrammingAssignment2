## The following code consists of a function that calculates
## the inverse of a square, invertible matrix. Also, there is
## a function used to cache the inverse of the square matrix
## to preclude repeated, lengthy matrix inversion calculations.

## makeCacheMatrix is a function that creates an object, involving
## a square, invertible matrix. It also creates a cache of the
## inverse matrix if it has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
  
        I <- NULL
        
        set <- function(y) {  # set matrix values

                x <<- y
                
                I <<- NULL

        }
        
        get <- function() x  # get matrix values
        
        setinv <- function(inverse) I <<- inverse  # set inverse value
        
        getinv <- function() I
        
        # get inverse value
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## cacheSolve is a function that calculates the inverse of the
## square, invertible matrix returned from the makeCacheMatrix
## function. If the inverse was previously calculated, the
## function retrieves it from the cache.

cacheSolve <- function(x, ...) {
  
        I <- x$getinv()  # check if inverse previously calculated
        
        if(!is.null(I)) {
          
                message("getting cached data")
                
                return(I)  # return cached inverse
                
        }
        
        data <- x$get()  # calculate matrix inverse
        
        I <- solve(data, ...)
        
        x$setinv(I)
        
        I  # Return a matrix that is the inverse of 'x'
        
}

