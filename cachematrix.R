## Assignment 2: Caching the Inverse of a Matrix
## May 2014 - R Programming (Coursera) 

## makeCacheMatrix function creates a "matrix" object that can
## cache its inverse.

## First line creates an empty matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Set the stored inverse value (s) to be NULL
  s <- NULL
  ## The following sets the value of the matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## and now we get the value of the matrix
  get <- function() x
  ## then we set the value of the inverse, using the solve function
  setInverse <- function(solve) s <<- solve
## and then we get the value of the inverse
  getInverse <- function() s
## finishes makeCacheMatrix by returning a list of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve determines whether to get the cached matrix
## (calculated by getInverse in makeCacheMatrix)
## and skip the computation,
## or to calculate the inverse of the matrix from scratch
## and sets the inverse value in the cache using "setInverse".

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## Check if the inverse is already cached
  s <- x$getInverse()
  ## Assumption testing - if "s" is not equal to NULL
  ## the inverse has been calculated so the cached matrix is used
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## If "s" = NULL then the matrix needs to be calculated
  ## First, get the matrix
  data <- x$get()
  ## then calculate the inverse
  s <- solve(data, ...)
  ## set the cache
  x$setInverse(s)
  ## return the inverse as the solution
  s
}
