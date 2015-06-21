#R Programming Course Assignment
#Part 1
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 j <- NULL # default value
  z <- NULL # default value
  setmatrix <- function(z) { #set the value of the matrix
    r <<- z ## cache the matrix 
    n <<- NULL # # sets the value of m as matrix inverse
  }
  list(setmatrix = setmatrix, getmatrix = getmatrix, # the functions
       setinverse = setinverse,
       getinverse = getinverse)
}
}
#Part 2
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
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
    n # return the inverse
}
