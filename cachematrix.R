# Get a list containing a function to set the value of the matrix, get the value of the matrix, 
# set the value of the inverse, and get the value of inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinversion <- function(inverse) inv <<- inverse
  getinversion  <- function() inv
  list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}

#Function returns the inverse of the matrix, without having to do a long computation if the inverse
#has already been computed.
#Matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}