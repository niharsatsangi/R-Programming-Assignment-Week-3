## Functions to cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initializing the inverse property
  inv <- NULL
  
  ## Method to set the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## Method to get the matrix
  get <- function(){
    ## returning matrix
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    ## storing inverse 
    inv <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## returns the inverse
    inv
  }
  
  ## Returns the list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##Computes the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cacheSolve" retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
       
   ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inv <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(inv)
  
  ## Return the matrix
  inv
  }
