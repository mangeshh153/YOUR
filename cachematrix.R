## Put comments here that give an overall description of what your
## functions do
## function written in partial fulfilment of the R-Programming language in Data Science
## week 3 assignment; week begining May 04, 2020; GitHub user: mangeshh153

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache its inverse
## define the argument with default mode of "matrix"
  inv <- NULL                                 ## initialize inv as NULL; will hold value of matrix              
       set <- function(y){                  ## define the set function to assign new
         x <<- y                            ## value of numeric vector in parent environment
         inv <<- NULL                         ## if there is a new matrix, reset inv to NULL
       }
      get <- function() x                   ## define the get function - returns value of the matrix argument
       setinverse <- function(inverse) inv <<- inverse  ## assign value of inverse in parent environment
       getinverse <- function() inv               ## gets the value of inverse where called       
       list(set = set, get = get,           ## you need this in order to refer to the functions with the $ operator
            setinverse = setinverse,               
            getinverse = getinverse)
       }


## Write a short comment describing this function
## this function computes the inverse of the special "matrix" returned by the makeCacheMatrix above
## if the inverse has already been calculated (and matrix has not changed), then cachSolve will retrive the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.NULL(inv)){
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    }
