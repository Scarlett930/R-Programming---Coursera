
makeCacheMatrix <- function(x = matrix()) {

inverse <- NULL
  
  set <- function(x) {
    my_matrix <<- x
    inverse <<- NULL
  }
  
  get <- function() return(my_matrix)
  
  setinv <- function(inv) inverse <<- inv
  
  getinv <- function() return(inverse)
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {

  inverse <- my_matrix$getinv()
  
  if(!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  data <- my_matrix$get()
  invserse <- solve(data, ...)
  my_matrix$setinv(inverse)
  inverse
  
}
