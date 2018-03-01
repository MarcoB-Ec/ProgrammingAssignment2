## Put comments here that give an overall description of what your
## functions do

## These couple of functions work together to calculate the inverse of a matrix saving computing
## time by caching inverse matrix calculation [as "values"]. 

#FUNCTION: "makeCacheMatrix" will create an square matrix 

## Write a short comment describing this function
##This function will create an square matrix and will return a list the "parameters"
## assigned to the object <inv>.

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

## This functions will evaluate if the inverse of a matriz has been calculated before. If so, it will
##return the value from its cache, otherwise it ill calculate it.


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

