# As described by the instructor, some functions need
# huge computations. In these cases, it is good save 
# the outputs, caching them, to  use in the future 
# avoiding recomputing. Matrix inversion is usually a
# costly computation. 

## makeCacheMatrix is a function that contains a list to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

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


## The following function calculates the inverse of the
## matrix created with the above function. It first checks
## if the inverse has already been calculated. If so, it
## gets the inverse from the cache and skips computation.
## Otherwise, it calculates the inverse of the matrix and 
## sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}