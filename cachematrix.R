## Put comments here that give an overall description of what your
## functions do

## This function allows us to create a matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(calculatedmatrix) inverse <<- calculatedmatrix
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## This function allows to calculate the inverse of the matrix we've created in 
#the previous function

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
    ## Return a matrix that is the inverse of 'x'
}