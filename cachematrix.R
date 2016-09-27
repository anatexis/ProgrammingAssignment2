## The following two functions enable us to calculate the inverse of a matrix. 
## Before calculating, the second function (cacheSolve) checks if a inverse is 
## stored in the first function (makeCacheMatrix). If so, it takes the result
## from there, if not it calculates it itself and stores the inverse in the 
## first function.


## There are a list of 4 functions stored in this function. To set a matrix, to 
## get the stored matrix out, to set a inverse of a matrix and to get the 
## inverse out of the function. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function checks if the getinv() from the function above is not NULL, 
## that would mean, that there has already been an inverse calculated. If 
## getinv() is not NULL it returns it with a message. If it is NULL it gets the
## matrix with get(), calculates the inverse and stores it in the other function
## with setinv()

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}
