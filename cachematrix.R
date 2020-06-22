## Matrix inversion is usually a costly computation.

##makeCacheMatrix: This function creates a special "matrix"
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invers <- NULL
        set <- function(y) {
                x <<- y
                invers <<- NULL
        }
        get <- function() x
        setinvers <- function(inverse) invers <<- inverse
        getinvers <- function() invers
        list(set = set, get = get, setinvers = setinvers, getinvers = getinvers)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invers <- x$getinvers()
        if(!is.null(invers)) {
                message("getting cached inverse matrix")
                return(invers)
        }
        data <- x$get()
        invers <- solve(data, ...)
        x$setinvers(invers)
        invers
}
