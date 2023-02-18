## Put comments here that give an overall description of what your
## functions do

## Two functions: makeCacheMatrix and makeCacheMatrix
##makeCacheMatrix consists of set, get, setinv, and getinv
##library(MASS) is used to calculate the inverse for non-squared and squared matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL   ##starting inverse as NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x     ##function to get the matrix x
setinv <- function(inverse) inv <<- inverse
getinv <- function() {
                      rox <- ginv(x)
                      rox %% x    ##function to calculate the inverse of the matrix
}
list(set = set,
     get = get,
     setinv = setinv,
     getinv = getinv)
}


## This function is used to find the cache data

cacheSolve <- function(x, ...) {  #gets the cache data
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {   ## checking if inverse is actually NULL
    message("getting cached data!")
    return(inv)       ## this returns the inverse value
  }
  han <- x$get()
  inv <- solve(han,...)     ## calculate the inverse value
  x$setInverse(inv)
  inv       ## Returns the inverse of 'x' as a matrix
}
