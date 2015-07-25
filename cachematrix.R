## The function takes an invertible matrix as an argument and returns a list of
## four functions: (1) set the value of the matrix, (2) get the value of the
## matrix, (3) compute and set the value of the inverse of the matrix, which is
## assumed to be invertible, and (4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes the list from the previous function as an argument and
## returns the inverse of the matrix. If the inverse has already been computed,
## i.e. the value of the inverse is not NULL, it will return this value and
## "exit" the function without computing it again. If it has not been computed
## previously it will load the data, invert the matrix and set the value of the
## inverse in the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
