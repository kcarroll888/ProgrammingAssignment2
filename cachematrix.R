# Two functions written for John Hopkins R Programming Course, week 3 assignment
#
# The first is a function 'factory'. It creates a function, which when called, returns
# a function which in turn caches the inverse of the matrix it is given

# The second function operates the matrix inverse function object. When called it
# checks to see if there is something in the matrix cache, if there is it will return this,
# otherwise it will call the created function and solves (inverts) the matrix supplied

# -----------------------------------------------------------------

# 1. makeCacheMatrix: This function creates a special "matrix"
#    object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    # set the inverse to null
    inv <- NULL
    
    # and the passed matrix to the matrix to be used in functions
    mat <- x
    
    # function to save the matrix to the cache
    saveMatrix <- function(x = matrix()) mat <<- x
    
    # function to save the solved matrix
    saveInverse <- function(x = matrix()) inv <<- x
    
    # function to get the original matrix from the cache
    getPrevMatrix <- function() mat
    
    # function to get the solved matrix from the cache
    getInverse <- function() inv
    
    # return the list of functions created
    list(saveMatrix=saveMatrix, saveInverse=saveInverse,
         getPrevMatrix=getPrevMatrix, getInverse=getInverse)
}

# 2. cacheSolve: This function computes the inverse of the
#    special "matrix" returned by makeCacheMatrix above.
#    If the inverse has already been calculated (and the matrix
#    has not changed), then the cachesolve should retrieve the
#    inverse from the cache.

cacheSolve <- function(x, ...) {
    
    # Get last matrix to be inverted from the cache
    y <- x$getPrevMatrix()
    
    # Get the inverse from the cache
    z <- x$getInverse()
    
    # If retrived isn't null
    if(!is.null(z)) {
        
        # Get the inverse from the cache, return it and exit
        inv <- x$getInverse()
        return(inv)
    }
    
    # This is a new matrix so solve it
    inv <- solve(y)
    
    # Save it original matrix
    x$saveMatrix(y)
    
    # Save the solved matrix
    x$saveInverse(inv)
    
    # Return the inverse
    inv
}