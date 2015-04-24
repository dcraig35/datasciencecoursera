## The following functions perform two tasks.  The first is to create a 
## matrix object that can cache it's inverse and the second is to compute the
## inverse of the matrix returned by the first function.  Note:  these 
## functions are currently intended to work with square matrices.

## The makeCacheMatrix function below creates a matrix object and can cache the
## inverse of that object.

makeCacheMatrix <- function(x = matrix()){
     
     m <- NULL
     
     set <- function(y) {
     
          x <<- y
          m <<- NULL
     
     }
     
     get <- function() x
     setinv <- function(inv) m <<- inv
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## The cacheSolve function computes the inverse of the matrix object returned
## by the makeCacheMatrix function above through the use of the solve function.
## The cacheSolve function will also check to see if the inverse has been 
## previously calculated and if so retrieves the cached value for the matrix.

cacheSolve <- function(x, ...) {
     
     m <- x$getinv()
     
     if(!is.null(m)) {
     
          message("Getting cached data.")
          return(m)
     
     }
     
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m
}