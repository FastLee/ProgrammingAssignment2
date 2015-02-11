## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  storedMatrix <- x
  inverseValue <- matrix()
  ## stored Matrix will hold the value of the matrix
  set <- function(y) {
    storedMatrix <<- y
    inverseValue <<- matrix()
    ## inverseValue will hold the cached value of the matrix
  }
  get <- function() storedMatrix
  setInverse <- function(inInverseValue) inverseValue <<- inInverseValue
  ## set the value of the calculated inverse for caching
  getInverse <- function() inverseValue
  ## get the value of the calculated inverse for caching
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  if (is.na(x$getInverse()[1,1])){
    x$setInverse(solve(x$get()))
    message("Calculating Inverse")
  } else {
    message("Cached Inverse")
  }
  return (x$getInverse())
}
