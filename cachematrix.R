## Put comments here that give an overall description of what your
## functions do

## the makeCacheMatrix function creates two matrix one stores the original matrix and one the inverse matrix.
## The function returns a list with 4 functions:
### set function - sets a new matrix to be used. The matrix is stored in the storedMatrix Variable
### get function - gets the matrix stored in the storedMatrix variable.
### setInverse function - sets the inverse matrix for caching. The inverse matrix is stored in the inverseValue variable.
### getInverse function - returns the cached inverse matrix stored in the inverseValue variable.

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


## cacheSolve function - accpets the special list x as a parametered (the list returned by the makeCachedMatrix)
## the function then checks whether there is a cached inverse matrix. If there is no cached matrix
## it caculates the inverse matrix and return the value. If there is a cached version it will return the cached inverse matrix.

cacheSolve <- function(x) {
  ## Checking whether there is a cached matrix. If there is no cached version. 
  ## If there is no cached version getInverse() will return an empty matrix and the value @ [1,1] will be NA.
  if (is.na(x$getInverse()[1,1])){
    ## calculating the inverse value and setting the cached matrix
    x$setInverse(solve(x$get()))
    message("Calculating Inverse")
  } else {
    message("Cached Inverse")
  }
  ## returning the inverse matrix
  return (x$getInverse())
}
