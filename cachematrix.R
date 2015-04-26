## Following functions together calculate inverse of a matrix
## and cache the resulted inverse matrix against input matrix in
## a special matrix that can cache its inverse
## to aviod the recalculation of matrix inverse repeatedly for the same given input.

## Assumption: Following functions assumes that the matrix supplied is always invertible

## The following function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # Holds inverse matrix
        set <- function(y) {
                x <<- y # Holds the input matrix
                m <<- NULL # Initializes the inverse matrix to NULL
        }
        get <- function() x # Returns the input matrix
        setinverse <- function(inverse) m <<- inverse # Sets inverse matrix to m
        getinverse <- function() m # Returns inverse matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) { ## If the inverse matrix is available in the cache, returns the cached value
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) ## Computes inverse of the matrix
        x$setinverse(m) ## Caches inverse of the matrix.
        m
}
