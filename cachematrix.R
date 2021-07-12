##Functions that cache the inverse of a matrix.
##1. makeCacheMatrix()->creates a special 'matrix' object that can cache its inverse.



## The function below, 'makeCacheMatrix', will create matrix caching an inverse.

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
       getinverse = getinverse) #Creates a list which assign each of these functions elements, then it returns it 
  #to the parent environment. We can now make use of the $.
  #This list will be used as the input to the function 'cacheSolve' below.
  
}

##2. cacheSolve()->computes the inverse of the 'matrix' in makeCacheMatrix().
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
