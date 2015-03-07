## Returns a list of functions to get and set
## the values of a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix
    invMatrix <- NULL

    # Sets the value of the matrix
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
    # Gets the value of the matrix
    get <- function() x

    # Sets the value of the inverse of the matrix
    setInverse <- function(inverse) invMatrix <<- inverse
    
    # Gets the value of the inverse of the matrix
    getInverse <- function() invMatrix

    # Return a list containing all of the defined functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse of the provided matrix x. This function
## uses caching, i. e. if the inverse has already been computed, the function
## will return the stored value of the computation (and will not perform it again)
cacheSolve <- function(x, ...) {
    invMatrix <- x$getInverse()

    # If the value is present, return it
    if (!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }

    # If the value is not present, calculate it and store it
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setInverse(invMatrix)
    
    # Return the inverse
    invMatrix
}
