## Author: Matt Way
## Date: November 18, 2018

## These functions will allow the user to create a special object
## which will store a matrix input by a user and cache its inverse.
## If the user requests the inverse of a matrix that has already been cached
## the function will return the cached value.


## The makeCacheMatrix function will create the a list of functions
## based on the matrix input by the user which can be called from the cache
## by other functions. The list of functions created by makeCacheMatrix includes
## a function to set the value of the matrix, return the matrix, set the inverse
## value, and return the inverse value.

makeCacheMatrix <- function(a = matrix()) {
      b <- NULL
      set <- function(c) {
            a <<- c
            b <<- NULL
      }
      get <- function() a
      setinverse <- function(solve) b <<- solve
      getinverse <- function() b
      list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}


## The cacheSolve function will allow the user to input the special object
## created by the makeCacheMatrix function. If the inverse of that matrix has
## been cached, that value will be returned, along with a message indicating
## this. Otherwise, the inverse of the matrix will be calculated and returned.

cacheSolve <- function(d, ...) {
      e <- d$getinverse()
      if(!is.null(e)) {
            message("getting cached data")
            return(e)
      }
      data <- d$get()
      e <- solve(data, ...)
      d$setinverse(e)
      e
}
