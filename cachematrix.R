## This program uses two functions which
## calculate the inverse of a matrix and store it in cache memory to avoid
## repeated calculation of the same inverse.

## The first function makeCacheMatrix  takes the matrix as an input and
## returns an object with four functions, 
## which set and get the input matrix and its inverse matrix. 
## It sets an initial NULL value to the inverse, also stores the inverse (calculated 
## in next function) in the cache. 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve takes the object from previous function as an  input
## and calcultes the inverse of the matrix, if it cannot retrive the 
## inverse from the cached data.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}