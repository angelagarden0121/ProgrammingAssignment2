## Coursera assignment: Create makeCacheMatrix() and cacheSolve()
## functions do: cache matrix's inverse and get the inverse

## There are two functions.  The first one is makeCacheMatrix to make a special matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
        	    x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## The second computes inverse of the above matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}
