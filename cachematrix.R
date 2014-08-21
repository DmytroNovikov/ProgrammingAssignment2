## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.


## The following function creates a special "matrix" object
## that can cache its inverse.
## For this assignment, assume that the matrix supplied is always
## invertible.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  
  setInverseMatrix <- function(invMatr) inverseMatrix <<- invMatr
  
  getInverseMatrix <- function() inverseMatrix
  
## IMHO commented version of 'getInverseMtrix' function is much 'better'
## from OOP principles.
## And we do not need 'setInverseMatrix' function in this case.
##
## For futher information you may follow the link:
##  https://class.coursera.org/rprog-006/forum/thread?thread_id=675
##
## getInverseMatrix <- function(...){
##    if(is.null(inverseMatrix)){
##      inverseMatrix <<- solve(x, ...)
##    } else {
##      message("getting cached data")
##    }
##    inverseMatrix
##  }
  
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## The following function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse
## has already been calculated (and the matrix has not changed),
## then `cacheSolve` retrieves the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix and sets
## the value in the cache via the `setInverseMatrix` function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  if(is.null(m)){
    m <- solve(x$get(), ...)
    x$setInverseMatrix(m)
  } else {
    message("getting cached data")
  }
  m
}
