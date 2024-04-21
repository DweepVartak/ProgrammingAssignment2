# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize a variable to store the inverse matrix
  inverse <- NULL
  
  # Setter function to set the matrix
  set <- function(y) {
    x <<- y
    # Reset the cached inverse
    inverse <<- NULL
  }
  
  # Getter function to get the matrix
  get <- function() x
  
  # Function to compute the inverse and cache it
  cacheInverse <- function() {
    if (!is.null(inverse)) {
      # Return the cached inverse if it exists
      return(inverse)
    }
    # Compute the inverse
    inverse <<- solve(x)
    # Return the computed inverse
    inverse
  }
  
  # Return a list of functions
  list(set = set, get = get, cacheInverse = cacheInverse)
}

# Function to compute the inverse of the special "matrix" object
# If the inverse has already been calculated (and the matrix has not changed), 
# then retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse
  inverse <- x$cacheInverse()
  # Return the cached inverse
  inverse
}
