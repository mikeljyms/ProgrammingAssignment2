# Programming Assignment 2: Lexical Scoping
# Coursera - R Programming 
# rprog-009

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize m global variable
  m <- NULL
  
  # Set function to reset global variables
  set <- function(y) {
    # Assign the value y to variable x that is set in the function's environment
    assign('x', y, envir = environment())
    # Assign NULL to variable m that exists in the Global environment
    assign('m', NULL, envir = .GlobalEnv)
  }
  
  # Get function to get the variable x set in the function's environment
  get <- function() mget('x',envir = environment())
  
  # Set function to change the current inverse matrix value of m in the global environment
  setmatrix <- function(solve) assign('m', solve, envir = .GlobalEnv)
  
  # Get function to get the current inverse matrix value in m the global environment
  getmatrix <- function() mget('m',envir = .GlobalEnv)
  
  # List to be able to call the function outside of the function
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  
  # Instantiate to f the makeCacheMatrix function
  f = makeCacheMatrix(x)
  
  # Get the current matrix existing in Global environment
  test <- f$getmatrix()
  
  # If there is an existing matrix and is not identical to the inverse of the input matrix,
  # call the set function to reset the variable of f
  if(!identical(test,solve(x))){
    f$set(x)
  }
  
  # If there is a defined matrix and the input matrix is the same as the cache matrix, return the
  # cache inverse matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Otherwise, solve for the inverse of input matrix x
  data <- f$get()
  m <- solve(data, ...)
  
  # Set the new inverse as the cached matrix
  f$setmatrix(m)
  
  # Return the inverse matrix m
  m
}
