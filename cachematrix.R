## Put comments here that give an overall description of what your
## functions do

## Makes a sudo copy of matrix with additional properties and functionalities
## Cached matrix keeps track of its inveres as well as provides a way to set
## and update the matrix and inverse
## Returns a list that points to the get and set functions

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  setMat <- function(n){
    m <<- n
    inv <<- NULL
  }
  getMat <- function(){
    m
  } 
  setInv <- function(i){
    inv <<- i
  }
  getInv <- function(){
    inv
  }
  
  list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}


## Function that takes in a cached matrix function (object?) and attempts to get
## the inverse. If the inverse was ran before and cached function return inverse
## from memory. Otherwise the cacheSolve function pulls the matrix from the list
## representing the cached matrix runs the inverse (solve) on it  and stores the
## results in the cached matrix list.

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'm'
  inv <- m$getInv()
  if(!is.null(inv)){ ## if inv is not null i.e has been cached
    message("Matrix inverese was cached. Retreving data now...")
    return(inv)
  }
  inputMatrix <- m$getMat()
  inv <- solve(inputMatrix, ...)
  m$setInv(inv)
  inv
}
