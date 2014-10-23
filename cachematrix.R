## These methods will inverse a matrix and cache it.  
##  If we have seen the matrix previously, the cached copy will be returned.


## This function will make a cached copy of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## calculates the inverse of the matrx
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list (set=set, get=get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## This will return a matrix that is the inverse of x.
##  If this matrix has been solve previously it will return the cached copy of it.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m))
  {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  ## calculates the inverse of the matrix
  m <- solve(data, ...)
  x$setmatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
