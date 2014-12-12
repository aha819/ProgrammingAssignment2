## Summary: These functions create a cache for a matrix and its inverse,
## calculate the given matrix's inverse, and allow users to access and reset
## values in the cache.

## makeCacheMatrix creates a "matrix" object that can cache a matrix and its
## inverse. The function accepts an input matrix and will return a list of
## functions you can call on to access values in the cache matrix.

## Note: Calling this function on a matrix will store the matrix, but will NOT
## solve the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
