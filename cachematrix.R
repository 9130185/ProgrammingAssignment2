##The first function,  makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean
## this function can cache its inverse:
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL   #initializing inerse as NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x   #function to get matrix x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() {
                inver <- ginv(x)
                inver%*%x   #function to obtain inverse of the matrix
        }
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {  #gets cache data
        inv <- x$getinv()
        if (!is.null(inv)) {  #checking whether inverse in NULL
                message("getting cached data")
                return(inv)   #return inverse value
        }
        data <- x$get()
        inv <- solve(data, ...)   #calculating inverse value
        x$setinv(inv)
        inv   #return a matrix that is the inverse of 'x'
}

f<- makeCacheMatrix(matrix(1:8, 2,4))
f$get()
f$getinv()
cacheSolve(f)
