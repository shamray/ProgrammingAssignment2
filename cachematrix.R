## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
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
