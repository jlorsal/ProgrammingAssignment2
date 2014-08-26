## Put comments here that give an overall description of what your
## functions do
#
# The main goal of this code is to show that an R object can be cached into a separate environment from
# the actual working environment to avoid potentially time-consuming computations if the object has already
# been processed and it did not change at all (i.e. statistical computations over a hughe unchanged data frame)
#
# These code contains two functions:
# - "makeCacheMatrix", to create a special "matrix" object that can cache the matrix inverse.
# - "cacheSolve", to compute the inverse of the special "matrix" returned by "makeCacheMatrix" function.


## Write a short comment describing this function
#
# "makecacheMatrix" is a function to create a special object. It is a list containing a function to: 
# (1) set the value of the matrix, (2) get the value of the matrix, (3) set the value of the matrix 
# inverse, and (4) get the value of the matrix inverse.
# The "<<-" operator is used here to assign a value to an object in an environment that is different from 
# the current environment. 

makeCacheMatrix <- function(X = matrix()) {
  m <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the matrix inverse
  setmatrix <- function(solve) m <<- solve
  
  # get the inverse matrix
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Write a short comment describing this function
#
# The "cacheSolve" function computes the inverse of the matrix created with the above function. 
# Firts, it checks whether the inverse of the matrix has already been computed. If so, it gets the 
# inverse matrix from the cache and skips the computation. Otherwise, it calculates the matrix inverse 
# and sets it in the cache via the "setmatrix" function.

cacheSolve <- function(x, ...) {
  # Get the matrix
  m <- x$getmatrix()
  
  if(!is.null(m)) {
    # If the inverse matrix was already calculated... take it from the cache
    message("Getting the cached matrix inverse")
    # Get "m"
    return(m)
  }
  
  # If the inverse matrix was not already cached... compute it
  # Use "get()" function as defined in the element of vector x
  matrix <- x$get()
  
  # Compute matrix inverse
  m <- solve(matrix, ...)
  
  # Use "setmatrix()" function
  x$setmatrix(m)
  m
}


# This is a code to test the above R chunks
# Test
v <- makeCacheMatrix()
data <- matrix(data = c(-1, -1, 2, 1), nrow = 2, ncol = 2)
data
v$set(data)
cacheSolve(v)
#      [,1] [,2]
# [1,]    1   -2
# [2,]    1   -1

# AB = I = BA?
A <- data
B <- cacheSolve(v)
C <- A*B
D <- B*A
c==D
#      [,1] [,2]
# [1,] TRUE TRUE
# [2,] TRUE TRUE

identical(C,D)
# [1] TRUE
