## Put comments here that give an overall description of what your
## functions do
## the fair of functions provide a cached version of inversing a matrix

## Write a short comment describing this function
## makeCachedMatrix create a special matrix which is a list 
## containing the four function of get/set the matrix, and 
## getinv/setinv to get or set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of a matrix while if a previous
## caculation is cached, it could use the cached result. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
