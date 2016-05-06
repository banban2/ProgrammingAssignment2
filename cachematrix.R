## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    getinverse <- function() inverse
    setinverse <- function(y) inverse <<- y
    get <- function() x
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse =getinverse)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
}
