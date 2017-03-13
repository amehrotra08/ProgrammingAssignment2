## The first function creats a special matrix
## object that can cache its inverse, as 
## requested in the assignment 

## This function follows the format of the given 
## examples.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}
}


## This function computes the inverse of the matrix 
## returned by makeCacheMatrix. If the inverse has 
## been calculated, then cacheSolve should retrieve it
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- makeCacheMatrix(data, ...)
        x$setinverse(m)
        m
}
