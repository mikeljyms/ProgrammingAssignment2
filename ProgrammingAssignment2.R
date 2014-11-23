makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse.
  m <- NULL
  
  #Set function to reset global variables.
  set <- function(y) {
    assign('x', y, envir = environment())
    assign('m', NULL, envir = .GlobalEnv)
  }
  
  #Get function to get the current m variable.
  get <- function() mget('x',envir = environment())
  
  #Set function to change the current inverse matrix value in m.
  setmatrix <- function(solve) assign('m', solve, envir = .GlobalEnv)
  
  #Get function to change the current inverse matrix value in m.
  getmatrix <- function() mget('m',envir = .GlobalEnv)
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


cacheSolve <- function(x, ...) {
  # This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  #If the inverse has already been calculated (and the matrix has not changed), 
  #then the cachesolve should retrieve the inverse from the cache
  
  f = makeCacheMatrix(x)
  test <- f$getmatrix()
  
  if(identical(test,solve(x))){
    f$set(x)
  }
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- f$get()
  m <- solve(data, ...)
  
  f$setmatrix(m)

  m
}


