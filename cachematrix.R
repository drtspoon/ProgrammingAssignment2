## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a special version of a matrix passed as an argument to this that can then be accessessed like a substantiated object in a parent environment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Write a short comment describing this function
## special version of the solve function that works on the special matrices to see if they are already substantiated in the global environment. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  #this all just follows the example code.  now how to make the inverse of a matrix?
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
