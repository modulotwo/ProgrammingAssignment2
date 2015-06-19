## Below are two functions 'makeCacheMatrix' and 'cacheSolve'
## that are used to create a special
## matrix object and cache its inverse. 



makeCacheMatrix <- function(m = matrix()) {
## This function creates a special "matrix" object
##      that can cache its inverse.
     mi <- NULL
     list(set = 
            function(x) {
              m <<- x
              mi <<- NULL },
          get = 
            function() m,
          setinv = 
            function(inv) {
              mi <<- inv },
          getinv = 
            function() mi
         ) 
}


cacheSolve <- function(m, ...) {
## This function computes the inverse of the special
##     "matrix" returned by `makeCacheMatrix`. 
##     If the inverse has already been calculated 
##     (and the matrix has not changed), then
##     `cacheSolve` retrieves the inverse from the cache.
            mi <- m$getinv()
            if(!is.null(mi)) {
                    message("getting cached data")
                    return(mi)
            }
            mat <- m$get()
            mi <- solve(mat, ...)
            m$setinv(mi)
            mi 
}

