## This R program contains two functions that compute and store the inverse matrix
## of an orginal input matrix.

## The first function (makeCacheMatrix) accepts a matrix as an input and returns 
## an object with additional functionality to be called from outside of the
## function's environment.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function (cacheSolve) takes the output
## from the first function and either computes the inverse matrix of the original
## or it looks up the already-computed inverse matrix from memory; either of which
## it returns.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
