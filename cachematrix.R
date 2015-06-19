## Below are two functions 'makeCacheMatrix' and 'cacheSolve'
## that are used to create a special
## matrix object and cache its inverse. 



makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object
##      that can cache its inverse.
     xi <- NULL
     set <- function(y) {
          x <<- y
          xi <<- NULL
     }
     get <- function() x
     setinv <- function(inv) xi <<- inv
     getinv <- function() xi 
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


cacheSolve <- function(x, ...) {
## This function computes the inverse of the special
##     "matrix" returned by `makeCacheMatrix`. 
##     If the inverse has already been calculated 
##     (and the matrix has not changed), then
##     `cacheSolve` retrieves the inverse from the cache.
            xi <- x$getinv()
            if(!is.null(xi)) {
                    message("getting cached data")
                    return(xi)
            }
            mat <- x$get()
            xi <- solve(mat, ...)
            x$setinv(xi)
            xi 
}

