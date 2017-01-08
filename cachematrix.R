## This R code have functions to solve the inverse of a matrix and to cache the result of this
## operation to avoid costly computation on a demand to solve the inverse for the same matrix

## This function is responsible for caching the inverse of a matrix to later use
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_matrix) m <<- inverse_matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute the inverse of a matrix or gets the inverse matrix from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting inverse matrix from cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
