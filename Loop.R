makeVector <- function(x = vector()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinversen = setinverse,
       getinverse = getinverse)
}

#cachemen

cachemean <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- inverse(data, ...)
  x$setinverse(i)
  i
}
