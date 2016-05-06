## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv.matrix <- NULL
        set <- function(y) {
                x <<- y
                inv.matrix <<- NULL
        }
        get <- function() x
        setinv <- function(invMatrix) inv.matrix <<- invMatrix
        getinv <- function() inv.matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m.solve <- x$getinv()
        if(!is.null(m.solve)) {
                message("getting cached data")
                return(m.solve)
        }
        data <- x$get()
        m.solve <- solve(data, ...)
        x$setinv(m.solve)
        m.solve
}
