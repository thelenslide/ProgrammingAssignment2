## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mtrx <- NULL
  set <- function(y) {
    x <<- y
    mtrx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mtrx <<- inverse
  getinverse <- function() mtrx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This is a function of getting the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtrx <- x$getinverse()
  if(!is.null(mtrx)) {
    message("getting cache inverse data")
    return(mtrx)
  } ##matrix is obtained from the subject
  data <- x$get()
  mtrx <- solve(data, ...) ##then calculates the inverse
  x$setinverse(mtrx)
  mtrx ##returns the matrix
}
