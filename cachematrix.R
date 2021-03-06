## Below is a pair of functions that cache the inverse of a matrix

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y){
      x <<- y
  }
  get <- function() x
  setinverse <- function(x) x_inverse <<- solve(x) 
  getinverse <- function() x_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$getinverse()
  if(!is.null(x_inverse)){
    message("getting cached data")
    return(x_inverse)
  }
  data <- x$get()
  x_inverse <- solve(data, ...)
  x$setinverse(x_inverse)
  x_inverse
}
