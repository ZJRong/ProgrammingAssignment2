## a pair of functions that cache the inverse of a matrix.

## creates a special "matrix" object that can cache its inverse.

# change x to m
makeCacheMatrix <- function(m = matrix()) {
    n <- NULL
    set <- function( matrix ) {
        m <<- matrix
        n <<- NULL
    }
    
    get <- function() m
    
    setInverse <- function(inverse) n <<- inverse
    
    getInverse <- function() n
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}

