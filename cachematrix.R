## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
## set/get functions are accessors for the matrix, any time set is called,
## the cached matrix inverse is reset
## set/get functions are accessors for the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(mean) m <<- mean
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## function computes the inverse of the special matrix contained in argument x
## if the inverse has been computed and a cached result is available, the cached
## result is used, if not the inverse is recomputed using solve() on special x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
