## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## the makeCacheMatrix is a custom matrix object with getters and setters object for 
## returning its inverse. This function does not actually calcuate the inverse, it simply
## stores the inverse in its setter method: setinv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() { x }
  getinv <- function() { inv }
  setinv <- function(inver) { inv <<- inver }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## the cacheSolve function does the inverse of a matrix. The requirement is that the 
## matrix should be a square matrix: 2x2, 4x4, 400x400 etc. After using solve() to inverse
## the matrix, the result is stored in the custom matrix object setter method which will 
## then be used via the getinv() method to return the result as "cache". In reality the 
## the browser cache is not used, instead a custom object getter and setter methods are 
## used very similar to a "class object" which defines its private and public variables
## similar to say a C++ or a C# language object. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("inverse cached value")
          return (inv)
        }
        else 
        {
          data <- x$get()
          inv <- solve(data)
          x$setinv(inv)
          return (inv)	
        }
}
