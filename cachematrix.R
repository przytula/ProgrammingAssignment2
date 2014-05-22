## --------------------------------------------------------------
## Peer Assessments : Programming Assignment 2: Lexical Scoping
## course : datascientist : module 2 : R programming
## --------------------------------------------------------------
## Caching the Mean of a Vector
## Below are two functions that are used to create a special object 
## that stores a numeric vector and cache's its mean.
## ---------------------------------------------------------------
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse using the solve function
## -----------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## ---------------------------------------------------------------------------
## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## ---------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
## ----------------------------------------------------
## end of assignment 
## ---------------------------------------------------