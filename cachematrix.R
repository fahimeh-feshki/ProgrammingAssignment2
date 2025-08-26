## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function to create a special "matrix" object that can cache its inverse
# This function creates a list of functions that can be used to set, get, and 
# cache the inverse of a matrix. It allows caching the inverse of the matrix 
# to avoid recomputing it multiple times.


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL  # Variable to store the cached inverse matrix
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y   # Assign the new matrix to x
    m <<- NULL # Reset the cached inverse when the matrix changes
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the value of the inverse of the matrix
  setInverse <- function(inverse) m <<- inverse
  
  # Function to get the cached inverse matrix
  getInverse <- function() m
  
  # Return a list of functions that can access and modify the matrix and its inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
# This function checks if the inverse of the matrix is already cached. If it is, 
# it returns the cached inverse. If not, it computes the inverse, caches it, and then 
# returns the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Retrieve the cached inverse matrix if it exists
  m <- x$getInverse()
  
  # If the inverse is cached, return it with a message indicating it's retrieved from cache
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If the inverse is not cached, compute it
  data <- x$get()  # Get the matrix
  m <- solve(data, ...)  # Compute the inverse using solve()
  
  # Cache the inverse matrix
  x$setInverse(m)
  
  # Return the computed inverse matrix
  m
  
}
