## The following pair of functions allows for the computation of matrix inversion...
    ##...with the use of cached data.

## makeCacheMatrix function returns a list containing functions...
## ...that set the matrix, get its value, set its inverse, and get this inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function takes as an argument the special "matrix" returned by...
    ##...the makeCacheMatrix function and returns its inverse.
## It checks whether there is cached inverse already;
    ## if there is none, then it proceeds to computing the inverse of the supplied matrix.

cacheSolve <- function(x, ...) {
    ## x is assumed to be an invertible matrix already;
        ## it is then intended that the code does not filter non-invertible matrices.
    
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
