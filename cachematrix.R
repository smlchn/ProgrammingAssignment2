## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that takes square matrix as input, it has 4 functions:, set, get, set_Inverse and get_inverse ##
## where these will return a list to makeCacheMatrix fucntion  ##

makeCacheMatrix <- function(x = matrix()) {  # x handles a matrix of square
        i <- NULL #inverse is null
        set <- function(y) {  #fed y into x, null into inverse
                x <<- y
                i <<- NULL 
        }
        get <- function() x  #return to x to process data
        set_Inverse <- function(inverse) i <<- inverse # mask i to inverse
        get_Inverse <- function() i #return to i
        list(set = set, get = get,
             set_Inverse = set_Inverse,
             get_Inverse = get_Inverse)
}



## Write a short comment describing this function
##cacheSolve is a function that takes in list return from makeCacheMatrix(), if matrix has been cached, matrix i will return else ##
## it will get matrix in x and fed into a variable, then return its inverse by using solve function and cach matrix ## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
             i <- x$get_Inverse()  #apply get_Inverse() function and fed into i
        if(!is.null(i)) {  #if matrix is not null, return value i to cacheSolve()
                message("getting cached data")
                return(i) 
        }
        data <- x$get()  #get x value and feed into data 
        i <- solve(data, ...) #solve return its inverse 
        x$set_Inverse(i) # set i matrix into x
        i #return to i 
}
