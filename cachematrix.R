##Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #Initialise the inverse property
  inv <- NULL
  
  #Set the matrix
  set <- function(y) {
    x <<-y
    inv <<- NULL
  }
  
  #Get the matrix
  get <- function() x
  
  #Set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  #Get the inverse
  
  getInverse <- function() inv
  
  #Return a list of the methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  #Return the inverse if it is already set
  if (!is.null(inv)) {
    message("getting cahed data")
    return(inv)
  }
  
  #Get the matrix 
  mat_dat <-x$get()
  
  #Find the inverse
  inv <- solve(mat_dat, ...)
  
  #Set the inverse
  x$setInverse(inv)
  
  #Return the inversed matrix
  inv 
}
