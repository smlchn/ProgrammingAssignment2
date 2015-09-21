## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_Inverse <- function(inverse) i <<- inverse
        get_Inverse <- function() i
        list(set = set, get = get,
             set_Inverse = set_Inverse,
             get_Inverse = get_Inverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
             i <- x$get_Inverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set_Inverse(i)
        i
}
