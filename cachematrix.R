## cachematrix.R
## 2018-06-23
## This file contains two methods, makeCacheMatrix and cacheSolve,
## which serve to facilitate caching matrix inverses to prevent
## excess memory consumption.

## makeCacheMatrix takes in a regular matrix and returns a list of
## four functions which return the matrix, change its value, return
## its inverse if it has one, and set its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve attempts to return the inverse of a cached matrix.
## If the cached matrix x already contains an inverse, that is returned.
## Else, the function stores the inverse into x and returns it.
## Should the inverse be incomputible, the function prints an error
## and sets the inverse as NA.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("found cache result")
    return(i)
  }
  
  mat <- x$get()
  inv <- tryCatch ({
    solve(mat, ...)
  },
  error=function(cond) {
    message("The function call produced an error:")
    message(cond)
    message("\nThis was likely because your matrix was not invertible.")
    
    NA
  })
  
  x$setinv(inv)
  
  inv
}
