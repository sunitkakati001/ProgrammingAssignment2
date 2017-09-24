## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Define function to set the value of the matrix. It also clears the old
  # inverse from the cache
  set <- function(y) {
    x <<- y    # Set the value
    m <<- NULL # Clear the cache
  }
  # Define function to get the value of the matrix
  get <- function() x
  # Define function to set the inverse. This is only used by getinverse() when
  # there is no cached inverse
  setInverse <- function(inverse) m <<- inverse
  # Define function to get the inverse
  getInverse <- function() m
  
  # Return a list with the above four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x) {
  m <- x$getInverse() # This fetches the cached value for the inverse
  if(!is.null(m)) { # If the cache was not empty, we can just return it
    message("getting cached data")
    return(m)
  }
  # The cache was empty. We need to calculate it, cache it, and then return it.
  data <- x$get()  # Get value of matrix
  m <- solve(data) # Calculate inverse
  x$setInverse(m)  # Cache the result
  m                # Return the inverse
}



#
# Test Results:
# 
# > m <- makeCacheMatrix()
# > m$set(matrix(c(0,2,2,0),2,2))
# > m$get()
# [,1] [,2]
# [1,]    0    2
# [2,]    2    0
# > cacheSolve(m)
#      [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0
# > cacheSolve(m)
# getting cached data
#      [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0
# > 
# 
# Change to matrix triggers fresh inversion solution:
#
# > m$set(matrix(c(100,2,2,100),2,2))
# > m$get()
# [,1] [,2]
# [1,]  100    2
# [2,]    2  100
# > cacheSolve(m)
# [,1]        [,2]
# [1,]  0.01000400 -0.00020008
# [2,] -0.00020008  0.01000400
# > cacheSolve(m)
# getting cached data
# [,1]        [,2]
# [1,]  0.01000400 -0.00020008
# [2,] -0.00020008  0.01000400
# 
# Change to matrix triggers fresh inversion solution:
#
# > m$set(matrix(c(0,2,2,0),2,2))
# > m$get()
# [,1] [,2]
# [1,]    0    2
# [2,]    2    0
# > cacheSolve(m)
# [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0
# > 
