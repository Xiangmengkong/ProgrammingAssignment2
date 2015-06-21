## First function set the matrix and calculate its inverse and get the inverse
## without caching. The second function returns the inverse with caching.

## This function sets the matrix, gets the matrix, sets its inverse and gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##This function returns the inverse. It first checks if the 
##inverse is aalready calculated: if it is, it returns the value;
##otherwise it will calculate and return it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("gives cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
