## The following functions together can calculate the inverse of a matrix but if the inverse is already 
## calculated, the cached value will be returned in order to save the time and the cost of computation.

## This function accepts a matrix as an argument and returns a list of four objects. Each
## object is a separate function as following:
## The first function sets the value of the matrix. The second function gets the value of the matrix. 
## The third function sets the value of the inverse of the matrix and the fourth function gets the value
## of the inverse of the function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## This function accepts an object in the form of the output of makeCacheMatrix. Then it calculates the inverse
## of the matrix. Before starting the calculation, it checks to see if the calculation has been done before. If 
## the inverse already exists, then the function returns the cached value and prints an appropriate message.


cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

