##Assignment 2 - for peer review

## Provide functionality to allow an inverse to be calulated once for a given matrix, 
##     and then cached for later retrieval
## Store values in parent environment to allow them to persist across calls


## Create a list object with methods to store and retrieve a matrix passed to it,
##      and to store and retrieve the inverse.
##   Set the inverse to be null when a new matrix is passed in.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse){
    m <<- inverse
  }
  getInverse <- function() {
    m
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of a given matrix
## Check if the inverse is already calculated, and if not, then calculate and store it.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
