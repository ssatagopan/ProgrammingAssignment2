## makeCacheMatrix returns a list of objects with relating to the
## matrix that was input
##
## cacheSolve returns the inverse of a matrix taking the output
## of makeCacheMatrix

## makeCacheMatrix takes in a matrix as input and returns a list of
## objects containing either a cached inverse value or a NULL

makeCacheMatrix <- function(x = matrix()) {
    
    # initializes cache
    inv <- NULL
    
    # sets value of matrix and initializes cache
    set <- function (y = matrix()){
        x <<- y
        inv <<- NULL
    }
    
    # gets matrix
    get <- function () x
    
    # sets cache value
    setInv <- function(invrs) inv <<- invrs
    
    # gets cache value
    getInv <- function() inv
    
    # returns objects
    list(set = set, get = get, setinv = setInv, getinv = getInv)
}


## Returns the inverse of the matrix used as input into makeCacheMatrix. Returns cached value
## if available, otherwise, calculates inverse, caches, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # gets cache value
    invrs <- x$getinv()
    
    # checks if cache has value and returns it if it does
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    
    # gets original matrix
    data <- x$get()
    
    # calculates inverse value
    invrs <- solve(data)
    
    # stores in cache
    x$setinv(invrs)
    
    # returns value
    invrs
}
