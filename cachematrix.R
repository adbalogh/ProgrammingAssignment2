## The below pair of functions support to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(y) inverseMatrix <<- y
        getInverse <- function() inverseMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## 'makeCacheMatrix' above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve retrieves the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverse()
        if(is.null(inverseMatrix)) {
                
                data <- x$get()
                
                ## Making sure 2nd argument is the identity matrix, in case
                ## there are multiple arguments supplied in 'cacheSolve' call.

                message("Computing inverse of the matrix...")
                identityMatrix <- diag(dim(x$get())[1])
                inverseMatrix <- solve(data, identityMatrix, ...)
                
                x$setInverse(inverseMatrix)
        }
        inverseMatrix
}