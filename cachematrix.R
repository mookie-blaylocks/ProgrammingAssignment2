## These functions work together to create cached matrix inversions.
## Rather than recalculate the solve() function each time the inverse is needed,
## the inverse is calculated upon first calling the getInverse() function within
## the makeCacheMatrix object. After this first call, the cached computed 
## inverse is used.

## matrix -> list((matrix->NULL),(NULL->matrix),(matrix->NULL),(NULL->matrix))
## makeCacheMatrix is a matrix-type object that allows for caching of the
## inverse by passing a matrix to makeCacheMatrix and then using it's function 
## list to access.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve builds on the makeCacheMatrix list by accessing the inverted 
## matrix as either the cached form (a true if statement) or computing the 
## inverse, if necessary. If the computation is done, then setInverse is
## is invoked and set to a non-null value so that the inverse is cached.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
