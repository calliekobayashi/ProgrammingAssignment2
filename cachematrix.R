## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x # get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse # set the value of the inverse
  getinverse <- function() inv # get the value of the inverse
  # return
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # inv function() 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # return will end the function
  }
  data <- x$get() # x function() 
  inv <- solve(data, ...) # calculates the inverse and stores it as inv
  x$setinverse(inv) # function(inverse) inv <<- inverse
  inv
}
