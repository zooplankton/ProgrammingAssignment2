## Put comments here that give an overall description of what your
## functions do
library(digest)
## Write a short comment describing this function
## Helper function for caching a matrix solution
## stores a checksum of the matrix the object is nitialized with
## returns special matrix object with functions
makeCacheMatrix <- function(x = matrix()) {
  soln <- NULL
  checksum <<- digest(x) 
  set <- function(y) {
    x <<- y
    soln <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setsoln <- function(newSoln) {
    soln <<- newSoln
  }
  
  getsoln <- function() {
    soln
  }
  
  getchecksum <- function() {
    checksum
  }
  
  out <- c(set = set, 
           get = get, 
           setsoln = setsoln,
           getsoln = getsoln,
           getchecksum = getchecksum)
  matrix(out)
}

## Write a short comment describing this function
## if a matrix isnt solved, solve and cache
## if a matrix is cached but it has changed, solve and cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x[[4]]()
  checksum <- x[[5]]()
  newchecksum <- digest(x[[2]]())
  if(!is.null(m) & identical(checksum,newchecksum))  {
    message("getting cached data")
    return(m)
  }
  data <- x[[2]]()
  m <- solve(data)
  x[[3]](m)
  m
}