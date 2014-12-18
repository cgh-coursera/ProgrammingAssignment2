## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix(X) takes X, a matrix-type object and returns a list
# consisting of the following objects:
# inv - a matrix which stores the inverse of X. If X has just changed, or is newly initialized, inv is set to NULL
# set(Y) - sets the matrix to Y
# setinv(Z) - sets the inverse of the matrix to Z
# getinv() - returns the matrix

# cacheSolve(X) reads X, a cached matrix object and returns its inverse.


# makeCacheMatrix(X) reads in a matrix and returns a list with
# methods and objects for the cached matrix object
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
	inv <- NULL
    }
    get <- function() x
    setinv <- function(invmatrix) inv <<- invmatrix
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve(X) reads X, a cached matrix object returned by makeCacheMatrix,
# and returns the inverse of X. The function will compute the inverse if
# it has not been stored in the cache. Otherwise, it will use the cached
# inverse and return it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
	return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
