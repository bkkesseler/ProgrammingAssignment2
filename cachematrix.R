## These two functions will, when combined, allow for
## the caching of the inverse of a matrix after
## calculation, to save computation time.

## This function creates a list with 4 items
## 1) A function that sets the value of a matrix
## 2) A function that gets the value of a matrix
## 3) A function that sets the inverse of a matrix
## 4) A function that gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks to see if a cache inverse of
## the provided matrix is available. If it is, the
## function retrieves the cached value. If not, the
## function calculates the inverse and stores it in
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
