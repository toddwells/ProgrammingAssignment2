# Cache the inverse of a matrix so it can be used repeatedly without having to recalculate

# Getter/Setter function to cache a Matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set new value of matrix and clear inverse value
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }

  # Get existing matrix
  get <- function() x
  
  # Cache a new inverse matrix
  setinverse <- function(inv) inv <<- inv
  
  # Get cached inverse
  getinverse <- function() inv

  # Return list of functions so they can be accessed externally using name$xxxx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Returns inverse of a matrix using the cached inverse if available. 
# If not, calculates the inverse and caches/returns it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(is.null(inv)){
    message("No cached inverse, calculating it")
    matr <- x$get()
    inv <- solve(matr)
    x$setinverse(inv)
  } else {
    message("Using cached inverse")
  } 
  inv
}
