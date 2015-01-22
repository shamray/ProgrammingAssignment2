## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverted <- function(value) {
    inverted <<- value
  }
  
  getInverted <- function() {
    inverted
  }
    
  list(set = set, get = get, setInverted = setInverted, getInverted = getInverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  result <- x$getInverted()
  if(!is.null(result)) {
    message("getting cached data")
    print(result)
    return(result)
  }
  
  originalMatrix <- x$get()
  invertedMatrix <- solve(originalMatrix)
  x$setInverted(invertedMatrix)
  
  invertedMatrix
}
