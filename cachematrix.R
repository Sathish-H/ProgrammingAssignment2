## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment

##This function creates a special "matrix" object
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # stores the cached value
  # initialize to NULL
  mat <- NULL
  
  # set the value of the matrix
  set <- function(s) {
    x <<- s
    mat <<- matrix()
  }
  
  # get the value of the matrix
  get <-function() x
  
  # invert the matrix and store in cache
  setInverse <- function(inv) mat <<- inv
  
  # get the inverted matrix from cache
  getInverse <- function() mat
  
  # return the created functions to the working environment
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache

cacheSolve <- function(x, ...) {
  ## attempt to get the inverse of the matrix stored in cache  
  mat <-x$getInverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if(!is.null(mat)) {
    message("getting cached data")
    
    # display matrix in console
    return(mat)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  
  # display matrix in console
  m
  ## Return a matrix that is the inverse of 'x'
  
  ## Sample run:
  ## > x = matrix(c(1,2,3,4),2,2)
  ## > m = makeCacheMatrix(x)
  ## > m$get()
  ##        [,1] [,2]
  ##[1,]    1    3
  ##[2,]    2    4
  
  ## No cache in the first run
  ## > cacheSolve(m)
  ##          [,1] [,2]
  ##[1,]   -2  1.5
  ##[2,]    1 -0.5
  
  ## Retrieving from the cache in the second run
  ## > cacheSolve(m)
  ## getting cached data.
  ##            [,1] [,2]
  ##[1,]   -2  1.5
  ##[2,]    1 -0.5
  ## > 
}
