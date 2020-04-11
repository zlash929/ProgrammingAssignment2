## makeCacheMatrix creates an R object that stores a Matrix and its inverse

makeCacheMatrix <- function(x = numeric()) { # Initialise objects
  i <- NULL
  set <- function(y) { # Assigns the input argument and clears cache value of i
    x <<- y
    i <<- NULL
  }
  get <- function() x # Extract output
  setinverse <- function(inverse) i <<- inverse # Assigns the input arguement in the cache
  getinverse <- function() i # Extract output
  list(set = set, get = get, # Creates a new object by returning a list
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve retrieves the inverse from the cached value that is stored in makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse() # Retrieve inverse
  if(!is.null(i)) { # Checks if already saved in cache
    message("getting cached data")
    return(i)
  }
  data <- x$get() # Retrieves matrix
  i <- solve(data, ...) # Calculates inverse
  x$setinverse(i) # Saves in cache
  i
}
