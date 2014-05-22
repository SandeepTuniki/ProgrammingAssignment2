
## The 2 functions 'makeCacheMatrix' and 'cacheSolve' will operate on matrix and
## it's inverse. The function 'makeCachematrix' caches the matrix and it's inverse,
## so that it need not be computed again when the inverse is required.
## The function 'cacheSolve' returns the cached inverse. If the cache is not 
## available, then it computes the inverse and caches it.



## This function takes a matrix as it's parameter and returns a list. The list 
## contains all the operations that can be performed on a matrix(set, get, setinv, getinv).

makeCacheMatrix <- function(x = matrix()) {
      #The matrix is intially not set. So, it's inverse it set to NULL.
      inv <- NULL
      
      #The 4 operations are defined here.
      
      set <- function(mat) {
            x <- mat
            inv <- NULL
      }
      
      get <- function() x
      
      setinv <- function(inverse) inv <- inverse
      
      getinv <- function() inv
      
      ## The list of operations are returned here.
      list( set = set, get = get, setinv = setinv, getinv = getinv )
}


## This function takes a list of operations that are returned by the 1st function.
## Then it returns the inverse of the matrix by either using the cached inverse,
## or by computing it dynamically.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      
      ## Check if the inverse if NULL. If it is not, then it's already cached. So, 
      ## simply return it.
      if( !is.null(inv) ) {
            message("getting cached inverse of the matrix")
            return (inv)
      }
      
      ## If it is not cached, the compute the inverse, cache it, and then return it.
      mat <- x$get()
      inv <- solve(mat,...)
      x$setinv(inv)
      inv
}











