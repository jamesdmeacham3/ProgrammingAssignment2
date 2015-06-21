#R Programming Course Assignment 2
#Part 1
#

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 j <- NULL # default value
  z <- NULL # default value
  setmatrix <- function(z) { #set the value of the matrix
    r <<- z # cache the matrix 
    n <<- NULL #sets the matrix makeCacheMatrix: 
  }
  list(	setmatrix = setmatrix, # the functions
	getmatrix = getmatrix, 
       	setinverse = setinverse,
       	getinverse = getinverse)
}
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
	 # Compare previous matrix
  n <- x$getinverse() # gets inverse if it's already been calculate
  if(!is.null(n)){ # has cacheSolve been run before?
    if(x$setmatrix() == x$getmatrix()) { # check for changes in the matrix
      #remove parts
      return(n)
    }
    # if it hasn't been run, then...
    z <- x$getmatrix() # what's the value of the input matrix?
    x$setmatrix(z) # go cache the inverse
    n <- solve(z, ...) # what is the inverse?
    x$setinverse(n) # go cache the inverse
    return(n) # return the inverse
}
