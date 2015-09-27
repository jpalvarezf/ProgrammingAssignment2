## Set of functions to make a matrix, solve its inverse and cache the result for further usage
## Derivative work from Roger Peng at https://github.com/rdpeng/ProgrammingAssignment2

## Usage example
## Make a new Matrix Object with makeCacheMatrix (the hilbert function creates a matrix, code taken form the help page of Solve)
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## myMatrix <- makeCacheMatrix(hilbert(8)); myMatrix
## Cache the result with cacheSolve (you can call the function serveral time to check if the Matrix was cached
## cacheSolve(myMatrix)
## You can verify if the result was successful by making this operation and getting the identity Matrix
## round(myMatrix$get() %*% cacheSolve(myMatrix), 3)

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
