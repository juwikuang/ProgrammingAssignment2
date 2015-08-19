## These two functions calculate and cache the inverse of matrices.


## Cache the matrix and its inverse
makeCacheMatrix <- function(x = numeric()) {
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

## return the cached inverse of a matrix, if not cached, calculate it.
cacheSolve <- function(x, ...) {
    ## Return the cached inverse of a matrix
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

