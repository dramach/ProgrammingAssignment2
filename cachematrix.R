## Put comments here that give an overall description of what your
## functions do
## CacheMatrix is a function that stores the inverse of a matrix. This is a 
## temporary store for data/computation which can be retrieved or accessed at 
## a later point in time.


## Write a short comment describing this function
## The CacheMatrix function generates a special matrix object that can cache
## the inverse of the matrix. We assign this to a different environment so that 
## it is available for retrieving later. The solve function used here generates 
## the inverse of the defined matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## the cacheSolve function computes the inverse of the matrix returned from 
## makeCacheMatrix.If the inverse has already been calculated and the matrix 
## unchanged the inverse is retrieved from the cache directly.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
