## 
## The makeCacheMatrix function recieves a matrix and can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL  ## sets the inverse to Null
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function() x 
      ## gets the matrix
      setInv <- function(solve) inv <<- solve
      ## rewrites the previous value of the inverse
      getInv <- function() inv
      ## getInv returns the value of the inverse
      list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
      ##returns the value of all the functions

}


## cacheSolve recieves a matrix and 
##can test whether the inverse is cached or not. If it is not cached
## a new inverse will be calculated

cacheSolve <- function(x, ...) {
      inv <- x$getInv()
      if(!is.null(inv)) {
              ## if the inverse is not NULL the inverse will be retrieved from cache
              message("getting cached data")
              return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)  
      ## if the inverse is NULL then the inverse will be recalculated
      x$setInv(inv)
      inv
      ## Returns a matrix that is the inverse of x
}
