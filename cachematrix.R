## This function gives and cache the inverse matrix of x. If x is not changed, 
## then the function will not recalculate the inserve matrix, it returns the cached one.

## 1st: create a square matrix (e.g. call this x). nrow() == ncol()
## 2nd: b <- makeCacheMatrix(X)
## 3rd: cacheSolve(b)

## if you want another inverse matrix, type: b$set(matrix(, ... ))
## and then type: cachemean(b)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This second function get the cached inverse matrix, and set to another if the matrix has changed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  if (nrow(data) == ncol(data) ) {m <- solve(data)
                                  x$setinverse(m)
                                  m
  } else {
    message("Matrix is not a square matrix")
  }
  
  #source("cacheSolve.R")
}
