

## Those two functions are two functions for caching the inverse of a matrix.


## makeCacheMatrix: The function creates a "matrix" object that could be used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # variable m tracks the value of the inverse of x.  
  m <- NULL 
  
  # The function named "set" modifys the value of x to be the value of y. 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # The function "get" returns the value of the matrix "x" that we are manipulating. 
  get <- function() x
  
  # The function "setinverse" modifies the value of the inverse of the matrix "x" that we are manipulating.
  setinverse <- function(inverseVal) m <<- inverseVal
  
  # The function "getinverse" returns the value of the inverse of the matrix "x" that we are manipulating.
  getinverse <- function() m
  # The list object is the returned value of the function call makeCacheMatrix. 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
  
}


## cacheSolve: The function solves the inqury of the inverse of the special "matrix" returned by makeCacheMatrix above. 
## In the case that the inverse has been calculated (not null), the funciton call of cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse() 
  ## x should store a list object as a resutl of a call of makeCacheMatrix.
  ## The list object has four methods which come with the indentical defining environment.
  ## if the inverse of the matrix is stored (not null), we just return it.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## comments for the remaining lines of code. 
  ## otherwise, we get the value of matrix we are manipulating, solve its inverse, store its inverse, and return the value of its reverse. 
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m 
}
