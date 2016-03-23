## Put comments here that give an overall description of what your
## functions doinv <- NULL
## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## 
##  Assign null value to vector doinv then write set and get functions, 
##  later call theses functions 
makeCacheMatrix <- function(x = matrix()) {
         doinv <- NULL
         set <- function(y) {
                x <<- y
                doinv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) doinv <<- inverse
        getInverse <- function() doinv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         doinv <- x$getInverse()
        if(!is.null(doinv)) {
                message("getting cached data")
                return(doinv)
        }
        data <- x$get()
        doinv <- inv(data, ...)
        x$setInverse(doinv)
        doinv
}
