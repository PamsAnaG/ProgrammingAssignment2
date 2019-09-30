## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  ## set matrix
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  ## get the matrix
  get <- function() x
  ##set inverse
  setInv <- function(i) invM <<- 1
  ## get inverse
  getInv <- function() invM
  ## methods
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMa <- x$getInv()
  if(!is.null(invMa)) {
    message("Getting cached inverse")
    return(invMa)
  }
  matrx <- x$get()
  invMa <- solve(matrx, ...)
  x$setmean(invMa)
  return(invMa)
}
