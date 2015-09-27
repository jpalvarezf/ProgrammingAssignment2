## Set of functions to make a matrix, solve its inverse and cache the result for further usage
## Derivative work from Roger Peng at https://github.com/rdpeng/ProgrammingAssignment2

## Function that creates a matrix with a set of getters and setters
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {                  ## Sets the initial value passed to the makeCacheMatrix
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x                   ## Gets and returns the value of the Matrix object
  setinverse <- function(inverse) inverseMatrix <<- inverse       ## Sets the inverse value passed to the makeCacheMatrix
  getinverse <- function() inverseMatrix                          ## Gets and returns the inverse value of the Matrix object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function that solves the inverse of a matrix and caches the result
cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {           ## Checks if the Matrix had been cached
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)       ## Solves the inverse of a Matrix
  x$setinverse(inverseMatrix)             
  inverseMatrix                           ## Returns de inverse of the Matrix
}
