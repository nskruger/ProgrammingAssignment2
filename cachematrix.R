## This pair of functions calculates the inverse
## of a matrix and caches the result 

##This function creates a matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
                        x <<-y
                        m <<-NULL
  }
  get<- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m 
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
    
}

##This function first checks if inverse of matrix returned by makeCacheMatrix
##has been calculated. If not then: 
## a) it calculates the inverse of the matrix and caches the inverse
## else b) it returns the inverse from the cache if matrix remained unchanged.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        
        
        if(!is.null(m)) {
              message("Getting Cached Data")
              return(m)
        
        }
        
        retInverted <- x$get()
        m <- solve(retInverted)
        x$setInverse(m)
        m
        
}

##The following example can be used to test functions:
##m <- matrix(c(-1, -2, 1, 1), 2,2)
##x <- makeCacheMatrix(m)
##mInv <- cacheSolve(x) ##calculates inverse
##mInv <- cacheSolve(x) ##retrieves inverse from cache
##print(mInv)

