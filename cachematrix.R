##The first function,  makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean
## this function can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if (!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
        
}