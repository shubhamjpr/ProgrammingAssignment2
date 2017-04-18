## inverse of matrix and it's cache

## function to find the inverse of matrix
##Here I have used the << operator to assign the value of an object in the cache.

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

## This function will check if the inverse of a matrix is calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix using solve function and sets the value of the inverse of the matrix in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
