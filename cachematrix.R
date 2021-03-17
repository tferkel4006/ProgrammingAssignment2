## These functions will invert an invertible matrix and store it in cache to be used in the case
## where the matrix needs to be inverted mulitple times to save processing time

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # initialize inverse to null
        invmat <- NULL
        #define the set function to cache the matrix provided
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        #return the the cached matrix
        get <- function() x
        #set the matrix inverse
        setinv <- function(solve) invmat <<- solve
        #return the matrix inverse that was cached
        getinv <- function() invmat
        #list of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invm <- x$getinv()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setinv(invm)
        invm
}