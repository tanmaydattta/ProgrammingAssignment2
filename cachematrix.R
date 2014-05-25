## Put comments here that give an overall description of what your
## functions do

## Custom cache matrix class to store the results of inverse calculate on matrix x

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function(y) x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() i
  list(set=set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The function does exactly as description. cache the result of solve if it does not have it
## (First time) it will calculate and store it for future

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getInverse()
  if(!is.null(inv)) {
    message("Inv has value getting it from the cache")
    return(inv)
  }
  data <-x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  return(inv)
}
