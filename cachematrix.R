## R PROGRAMMING WEEK 3 ASSIGNMENT 
## SET OF FUNCTIONS THAT ALLOW FOR CACHING OF INVERTED MATRIX OR CALCULATING 
## INVERSE OF MATRIX IF IT DOES NOT ALREADY EXIST 

## creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getMatrix <- function() x 
    setInverse <- function(myinverse) m <<- myinverse 
    getInverse <- function() m 
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, 
         getInverse = getInverse)
}


## computes the inverse of the matrix returned in makeCacheMatrix if it 
## hasn't already been calculated - if so, retrieves matrix from cache 

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$getMatrix()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}


