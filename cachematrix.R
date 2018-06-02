## Programming Assignment 2 (Week 3)
## D. Singletary
## 6/2/18
## Demonstrate lexical scoping using inverse matrix cache functions

## makeCacheMatrix creates a list of functions to cache inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(matrix) m <<- matrix
    getMatrix <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


## cacheSolve uses result of makeCacheMatrix to calculate or use cached inverse value

cacheSolve <- function(x, ...) {
    m <- x$getMatrix()
    if (!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setMatrix(m)
    m
}