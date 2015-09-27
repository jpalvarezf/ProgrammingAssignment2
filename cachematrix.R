## Set of functions to make a matrix and cache it
## Derivative work from Roger Peng at https://github.com/rdpeng/ProgrammingAssignment2

## Function that creates a matrix with a set of getters and setters

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function that solves the inverse of a matrix and caches the result

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {           ##Checks if the Matrix had been cached
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)       ##Solves the inverse of a Matrix
  x$setinverse(inverseMatrix)             
  inverseMatrix                           ##Returns de inverse of the Matrix
}
