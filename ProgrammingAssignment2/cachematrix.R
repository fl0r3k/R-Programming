## Following functions calculate and cache inverse of square matrix

## makeCacheMatrix function creates a special matrix object
## that can cache its inverse
##
## arguments:
## x - square inversible matrix
##
## returns:
## Function will return list of functins associated with supplied matrix
## that can return matrix, change matrix and compute inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    # seting inverse of matrix to null
    i <- NULL
    # sets matrix supplied as argument
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # returns supplied matrix
    get <- function() x
    # sets calculated inverse of matrix - caching
    setinverse <- function(inverse) i <<- inverse
    # returns cached inverse of matrix
    getinverse <- function() i
    # returns special matrix object as list of functions associated with supplied matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve function returns computed inverse of special matrix from cache
## if computed or calls solver to return inverse of matrix then caches it.
##
## arguments:
## x - special matrix returned by makeCacheMatrix function
##
## returns:
## matrix that is inverse to supplied as argument if inverse can be computed

cacheSolve <- function(x, ...) {
    # tries to pull cached inverse of matrix
    i <- x$getinverse()
    # if inverse is cached returns inverse of matrix
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # if inverse is not cached calucaltes it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    # return cached inverse
    i
}
