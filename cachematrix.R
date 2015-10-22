## These function pairs allow a user to derive an inverse of a matrix 
## when the initial matrix is provided.  Note: the matrix must be a 
## square, invertible matrix in order for this function to complete.


## makeCacheMatrix is used to create the matrix (x).
## This matrix is later converted into a data frame so that the
## inverse of that data frame can be derived.

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        setmx <- function(y) {
                x <<- y
                inv <<- matrix()
        }
        getmx <- function() x
        setinv <- function(invs) inv <<- invs
        getinv <- function() inv
        list(setmx = setmx, getmx = getmx,
             setinv = setinv,
             getinv = getinv)
}

## cachSolve generates the inverse of the provided matrix while also Caching
## the results so that they can be used later.

cacheSolve <- function(z, ...) {
        inv <- z$getinv()
        if(!anyNA(inv)) {
                message("getting cached data")
                return(inv)
        } 
                data <- as.data.frame(z$getmx())
                inv <- solve(data, ...)
                z$setinv(inv)
                as.matrix(inv)
}
