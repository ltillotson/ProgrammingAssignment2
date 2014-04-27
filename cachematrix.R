## The following functions enable us to cache the inverse of a matrix
## so as not to waste time re-computing it if it has already been computed

## makeCacheMatrix sets up a special matrix with functions attached
## to it that can be used to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}



## cacheSolve checks to see if a matrix has already been inverted,
## gets the inverse if it has, and computes and caches it if it has not.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if (!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

## Delete all this before submitting!

matrix1 <- matrix(1:4, 2)
makeCacheMatrix(matrix1)
cacheSolve(makeCacheMatrix(matrix1))



