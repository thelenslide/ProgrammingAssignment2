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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtrx <- x$getinverse()
  if(!is.null(mtrx)) {
    message("getting cache inverse data")
    return(mtrx)
  }
  data <- x$get()
  mtrx <- solve(data, ...)
  x$setinverse(mtrx)
  mtrx
}
