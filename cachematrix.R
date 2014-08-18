## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(assignValue) {
        x <<- assignValue
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## It first checks if the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Now getting cached data...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
