## Put comments here that give an overall description of what your
## functions do: 
## -makeCacheMatrix recieve a matrix and stores it and its inverse after first calculus
## -cacheSolve used to retrieves the Inverse Matrix, if it already exist return the value in cache

## Write a short comment describing this function
## -Receive a matrix and stores it in X
## -The Set method update X and set matrixInv to null, so every update in the 
##  matrix will clean the matrixInv
## -The Get method returns the original matrix
## -The SetInverse is used by cacheSolve, it set the Inverse of the original matrix
## -The GetInverse retrieve the inversed matrix, also used by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  matrixInv <- NULL
  set <- function(y) {
    x <<- y
    matrixInv <<- NULL
  }
  get <- function() x
  setInverse <- function(mI) matrixInv <<- mI
  getInverse <- function() matrixInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## -Checks if already exists a value for the Inverse Matrix:
## --If there is a value stored, return it
## --If it is null, create the inverse matrix, store it using SetInverse method 
##   and return the value 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixInv <- x$getInverse()
  if(!is.null(matrixInv)) {
    message("getting cached data")
    return(matrixInv)
  }
  data <- x$get()
  matrixInv <- solve(data, ...)
  x$setInverse(matrixInv)
  matrixInv
}
