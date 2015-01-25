## The pair of  functions in this file are used to compute the inverse of a matrix.
## The computation result is being cached, i.e. if the inverse has already been found and
## the matrix hasn't changed, the computation result is returned without the need to 
## recalculate the inverse.

## You need first to create a "matrix" object with makeCacheMatrix() and then you can use
## cacheSolve(x) to find the inverse.



## This function creates a "matrix" object that is able to cache its inverse. The "matrix"
## object is really a list of functions set(), get(), setInverted() and getInverted()

makeCacheMatrix <- function(original = matrix()) {
  inverted <- NULL
  
  set <- function(y) {
    original <<- y
    inverted <<- NULL
  }
  
  get <- function() {
    original
  }
  
  setInverted <- function(value) {
    inverted <<- value
  }
  
  getInverted <- function() {
    inverted
  }
    
  list(set = set, get = get, setInverted = setInverted, getInverted = getInverted)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  result <- x$getInverted()
  
  ## Return cached matrix
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  
  ## Calculate inverse matrix 
  originalMatrix <- x$get()
  invertedMatrix <- solve(originalMatrix)
  x$setInverted(invertedMatrix)
  
  invertedMatrix
}
